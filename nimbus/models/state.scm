;;; -*- mode: scheme; -*-
;;; state.scm - State management models for Nimbus IAC Platform

(define-module (nimbus models state)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 hash-table)
  #:use-module (gcrypt hash)
  #:use-module (json)
  #:export (<resource-status>
            <resource>
            <state-snapshot>
            <state>
            make-resource
            make-state-snapshot
            make-state
            resource-id
            resource-state-id
            resource-type
            resource-name
            resource-properties
            resource-dependencies
            resource-status
            resource-created-at
            resource-updated-at
            state-snapshot-id
            state-snapshot-state-id
            state-snapshot-data
            state-snapshot-operation-type
            state-snapshot-created-at
            state-id
            state-stack-name
            state-environment
            state-resources
            state-outputs
            state-checksum
            state-created-at
            state-updated-at
            state-version
            calculate-checksum
            add-resource
            get-resource
            update-resource
            remove-resource))

;; Resource Status Enumeration
(define-class <resource-status> ()
  (value #:init-keyword #:value
         #:getter resource-status-value))

(define resource-status-pending (make <resource-status> #:value 'pending))
(define resource-status-creating (make <resource-status> #:value 'creating))
(define resource-status-created (make <resource-status> #:value 'created))
(define resource-status-updating (make <resource-status> #:value 'updating))
(define resource-status-deleting (make <resource-status> #:value 'deleting))
(define resource-status-deleted (make <resource-status> #:value 'deleted))
(define resource-status-failed (make <resource-status> #:value 'failed))

;; Resource Class
(define-class <resource> ()
  (id #:init-keyword #:id
      #:getter resource-id)
  (state-id #:init-keyword #:state-id
            #:getter resource-state-id)
  (resource-type #:init-keyword #:resource-type
                 #:getter resource-type)
  (resource-name #:init-keyword #:resource-name
                 #:getter resource-name)
  (properties #:init-keyword #:properties
              #:init-form (make-hash-table)
              #:getter resource-properties)
  (dependencies #:init-keyword #:dependencies
                #:init-form '()
                #:accessor resource-dependencies)
  (status #:init-keyword #:status
          #:init-form resource-status-pending
          #:accessor resource-status)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter resource-created-at)
  (updated-at #:init-keyword #:updated-at
              #:init-form (current-date)
              #:accessor resource-updated-at))

;; State Snapshot Class
(define-class <state-snapshot> ()
  (id #:init-keyword #:id
      #:getter state-snapshot-id)
  (state-id #:init-keyword #:state-id
            #:getter state-snapshot-state-id)
  (snapshot-data #:init-keyword #:snapshot-data
                 #:init-form (make-hash-table)
                 #:getter state-snapshot-data)
  (operation-type #:init-keyword #:operation-type
                  #:getter state-snapshot-operation-type)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter state-snapshot-created-at))

;; State Class
(define-class <state> ()
  (id #:init-keyword #:id
      #:getter state-id)
  (stack-name #:init-keyword #:stack-name
              #:getter state-stack-name)
  (environment #:init-keyword #:environment
               #:getter state-environment)
  (resources #:init-keyword #:resources
             #:init-form (make-hash-table)
             #:accessor state-resources)
  (outputs #:init-keyword #:outputs
           #:init-form (make-hash-table)
           #:accessor state-outputs)
  (checksum #:init-keyword #:checksum
            #:init-form #f
            #:accessor state-checksum)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter state-created-at)
  (updated-at #:init-keyword #:updated-at
              #:init-form (current-date)
              #:accessor state-updated-at)
  (version #:init-keyword #:version
           #:init-form 1
           #:accessor state-version))

;; Constructor functions
(define* (make-resource #:key id state-id resource-type resource-name 
                        (properties (make-hash-table))
                        (dependencies '())
                        (status resource-status-pending))
  "Create a new resource instance"
  (make <resource>
    #:id id
    #:state-id state-id
    #:resource-type resource-type
    #:resource-name resource-name
    #:properties properties
    #:dependencies dependencies
    #:status status))

(define* (make-state-snapshot #:key id state-id 
                              (snapshot-data (make-hash-table))
                              operation-type)
  "Create a new state snapshot instance"
  (make <state-snapshot>
    #:id id
    #:state-id state-id
    #:snapshot-data snapshot-data
    #:operation-type operation-type))

(define* (make-state #:key id stack-name environment 
                     (resources (make-hash-table))
                     (outputs (make-hash-table))
                     (checksum #f)
                     (version 1))
  "Create a new state instance"
  (make <state>
    #:id id
    #:stack-name stack-name
    #:environment environment
    #:resources resources
    #:outputs outputs
    #:checksum checksum
    #:version version))

;; Methods for State class
(define-method (calculate-checksum (state <state>))
  "Calculate SHA256 checksum of current state"
  (let* ((resources-alist (hash-map->list cons (state-resources state)))
         (outputs-alist (hash-map->list cons (state-outputs state)))
         (state-data (list (cons 'resources resources-alist)
                          (cons 'outputs outputs-alist)))
         (json-string (scm->json-string state-data)))
    (bytevector->base16-string
     (sha256 (string->utf8 json-string)))))

(define-method (add-resource (state <state>) (resource <resource>))
  "Add a resource to the state"
  (hash-set! (state-resources state) 
             (resource-id resource) 
             resource)
  (set! (state-updated-at state) (current-date))
  (set! (state-version state) (+ 1 (state-version state))))

(define-method (get-resource (state <state>) resource-id)
  "Get a resource from the state by ID"
  (hash-ref (state-resources state) resource-id))

(define-method (update-resource (state <state>) (resource <resource>))
  "Update an existing resource in the state"
  (when (hash-ref (state-resources state) (resource-id resource))
    (set! (resource-updated-at resource) (current-date))
    (hash-set! (state-resources state) 
               (resource-id resource) 
               resource)
    (set! (state-updated-at state) (current-date))
    (set! (state-version state) (+ 1 (state-version state)))))

(define-method (remove-resource (state <state>) resource-id)
  "Remove a resource from the state"
  (hash-remove! (state-resources state) resource-id)
  (set! (state-updated-at state) (current-date))
  (set! (state-version state) (+ 1 (state-version state))))