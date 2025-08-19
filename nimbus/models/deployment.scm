;;; -*- mode: scheme; -*-
;;; deployment.scm - Deployment history models for Nimbus IAC Platform

(define-module (nimbus models deployment)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 hash-table)
  #:export (<deployment-status>
            <deployment-type>
            <deployment-step>
            <deployment-artifact>
            <deployment>
            make-deployment-step
            make-deployment-artifact
            make-deployment
            deployment-status-pending
            deployment-status-running
            deployment-status-completed
            deployment-status-failed
            deployment-status-rolled-back
            deployment-type-create
            deployment-type-update
            deployment-type-delete
            deployment-type-rollback
            deployment-step-id
            deployment-step-deployment-id
            deployment-step-name
            deployment-step-resource-id
            deployment-step-action
            deployment-step-status
            deployment-step-input-params
            deployment-step-output-data
            deployment-step-started-at
            deployment-step-completed-at
            deployment-step-error-message
            deployment-artifact-id
            deployment-artifact-deployment-id
            deployment-artifact-type
            deployment-artifact-uri
            deployment-artifact-checksum
            deployment-artifact-size-bytes
            deployment-artifact-created-at
            deployment-id
            deployment-stack-id
            deployment-environment
            deployment-deployer-id
            deployment-type
            deployment-configuration
            deployment-status
            deployment-started-at
            deployment-completed-at
            deployment-error-message
            deployment-steps
            deployment-artifacts
            add-step
            add-artifact
            complete-deployment
            start-step
            complete-step
            fail-step))

;; Deployment Status Enumeration
(define-class <deployment-status> ()
  (value #:init-keyword #:value
         #:getter deployment-status-value))

(define deployment-status-pending (make <deployment-status> #:value 'pending))
(define deployment-status-running (make <deployment-status> #:value 'running))
(define deployment-status-completed (make <deployment-status> #:value 'completed))
(define deployment-status-failed (make <deployment-status> #:value 'failed))
(define deployment-status-rolled-back (make <deployment-status> #:value 'rolled-back))

;; Deployment Type Enumeration
(define-class <deployment-type> ()
  (value #:init-keyword #:value
         #:getter deployment-type-value))

(define deployment-type-create (make <deployment-type> #:value 'create))
(define deployment-type-update (make <deployment-type> #:value 'update))
(define deployment-type-delete (make <deployment-type> #:value 'delete))
(define deployment-type-rollback (make <deployment-type> #:value 'rollback))

;; Deployment Step Class
(define-class <deployment-step> ()
  (id #:init-keyword #:id
      #:getter deployment-step-id)
  (deployment-id #:init-keyword #:deployment-id
                 #:getter deployment-step-deployment-id)
  (step-name #:init-keyword #:step-name
             #:getter deployment-step-name)
  (resource-id #:init-keyword #:resource-id
               #:getter deployment-step-resource-id)
  (action #:init-keyword #:action
          #:getter deployment-step-action)
  (status #:init-keyword #:status
          #:init-form deployment-status-pending
          #:accessor deployment-step-status)
  (input-params #:init-keyword #:input-params
                #:init-form (make-hash-table)
                #:accessor deployment-step-input-params)
  (output-data #:init-keyword #:output-data
               #:init-form (make-hash-table)
               #:accessor deployment-step-output-data)
  (started-at #:init-keyword #:started-at
              #:init-form #f
              #:accessor deployment-step-started-at)
  (completed-at #:init-keyword #:completed-at
                #:init-form #f
                #:accessor deployment-step-completed-at)
  (error-message #:init-keyword #:error-message
                 #:init-form #f
                 #:accessor deployment-step-error-message))

;; Deployment Artifact Class
(define-class <deployment-artifact> ()
  (id #:init-keyword #:id
      #:getter deployment-artifact-id)
  (deployment-id #:init-keyword #:deployment-id
                 #:getter deployment-artifact-deployment-id)
  (artifact-type #:init-keyword #:artifact-type
                 #:getter deployment-artifact-type)
  (artifact-uri #:init-keyword #:artifact-uri
                #:getter deployment-artifact-uri)
  (checksum #:init-keyword #:checksum
            #:getter deployment-artifact-checksum)
  (size-bytes #:init-keyword #:size-bytes
              #:getter deployment-artifact-size-bytes)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter deployment-artifact-created-at))

;; Deployment Class
(define-class <deployment> ()
  (id #:init-keyword #:id
      #:getter deployment-id)
  (stack-id #:init-keyword #:stack-id
            #:getter deployment-stack-id)
  (environment #:init-keyword #:environment
               #:getter deployment-environment)
  (deployer-id #:init-keyword #:deployer-id
               #:getter deployment-deployer-id)
  (deployment-type #:init-keyword #:deployment-type
                   #:accessor deployment-type)
  (configuration #:init-keyword #:configuration
                 #:init-form (make-hash-table)
                 #:accessor deployment-configuration)
  (status #:init-keyword #:status
          #:init-form deployment-status-pending
          #:accessor deployment-status)
  (started-at #:init-keyword #:started-at
              #:init-form (current-date)
              #:accessor deployment-started-at)
  (completed-at #:init-keyword #:completed-at
                #:init-form #f
                #:accessor deployment-completed-at)
  (error-message #:init-keyword #:error-message
                 #:init-form #f
                 #:accessor deployment-error-message)
  (steps #:init-keyword #:steps
         #:init-form '()
         #:accessor deployment-steps)
  (artifacts #:init-keyword #:artifacts
             #:init-form '()
             #:accessor deployment-artifacts))

;; Constructor functions
(define* (make-deployment-step #:key id deployment-id step-name 
                                resource-id action
                                (status deployment-status-pending)
                                (input-params (make-hash-table))
                                (output-data (make-hash-table)))
  "Create a new deployment step"
  (make <deployment-step>
    #:id id
    #:deployment-id deployment-id
    #:step-name step-name
    #:resource-id resource-id
    #:action action
    #:status status
    #:input-params input-params
    #:output-data output-data))

(define* (make-deployment-artifact #:key id deployment-id artifact-type 
                                   artifact-uri checksum size-bytes)
  "Create a new deployment artifact"
  (make <deployment-artifact>
    #:id id
    #:deployment-id deployment-id
    #:artifact-type artifact-type
    #:artifact-uri artifact-uri
    #:checksum checksum
    #:size-bytes size-bytes))

(define* (make-deployment #:key id stack-id environment deployer-id 
                          deployment-type
                          (configuration (make-hash-table))
                          (status deployment-status-pending))
  "Create a new deployment"
  (make <deployment>
    #:id id
    #:stack-id stack-id
    #:environment environment
    #:deployer-id deployer-id
    #:deployment-type deployment-type
    #:configuration configuration
    #:status status))

;; Methods for Deployment
(define-method (add-step (deployment <deployment>) (step <deployment-step>))
  "Add a deployment step to the deployment"
  (set! (deployment-steps deployment)
        (append (deployment-steps deployment) (list step))))

(define-method (add-artifact (deployment <deployment>) (artifact <deployment-artifact>))
  "Add an artifact to the deployment"
  (set! (deployment-artifacts deployment)
        (append (deployment-artifacts deployment) (list artifact))))

(define-method (complete-deployment (deployment <deployment>) status #:key error-message)
  "Mark deployment as complete with the given status"
  (set! (deployment-status deployment) status)
  (set! (deployment-completed-at deployment) (current-date))
  (when error-message
    (set! (deployment-error-message deployment) error-message)))

;; Methods for Deployment Step
(define-method (start-step (step <deployment-step>))
  "Mark a deployment step as started"
  (set! (deployment-step-status step) deployment-status-running)
  (set! (deployment-step-started-at step) (current-date)))

(define-method (complete-step (step <deployment-step>) #:key (output-data #f))
  "Mark a deployment step as completed successfully"
  (set! (deployment-step-status step) deployment-status-completed)
  (set! (deployment-step-completed-at step) (current-date))
  (when output-data
    (set! (deployment-step-output-data step) output-data)))

(define-method (fail-step (step <deployment-step>) error-message)
  "Mark a deployment step as failed"
  (set! (deployment-step-status step) deployment-status-failed)
  (set! (deployment-step-completed-at step) (current-date))
  (set! (deployment-step-error-message step) error-message))

;; Helper function to get all steps with a specific status
(define-method (get-steps-by-status (deployment <deployment>) status)
  "Get all deployment steps with the specified status"
  (filter (lambda (step)
            (eq? (deployment-status-value (deployment-step-status step))
                 (deployment-status-value status)))
          (deployment-steps deployment)))

;; Helper function to check if deployment is in progress
(define-method (deployment-in-progress? (deployment <deployment>))
  "Check if the deployment is currently in progress"
  (eq? (deployment-status-value (deployment-status deployment))
       'running))