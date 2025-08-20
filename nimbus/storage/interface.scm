;;; -*- mode: scheme; -*-
;;; interface.scm - Storage backend interface for Nimbus IAC Platform

(define-module (nimbus storage interface)
  #:use-module (oop goops)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:export (<storage-backend>
            save-state
            load-state
            delete-state
            list-states
            save-secret
            load-secret
            delete-secret
            list-secrets
            save-deployment
            load-deployment
            list-deployments
            save-policy
            load-policy
            delete-policy
            list-policies
            begin-transaction
            commit-transaction
            rollback-transaction
            lock-resource
            unlock-resource
            storage-backend-name
            storage-backend-config))

;; Abstract Storage Backend Class
(define-class <storage-backend> ()
  (name #:init-keyword #:name
        #:getter storage-backend-name)
  (config #:init-keyword #:config
          #:init-form (make-hash-table)
          #:getter storage-backend-config))

;; Generic methods for state management
(define-generic save-state)
(define-generic load-state)
(define-generic delete-state)
(define-generic list-states)

;; Generic methods for secret management
(define-generic save-secret)
(define-generic load-secret)
(define-generic delete-secret)
(define-generic list-secrets)

;; Generic methods for deployment management
(define-generic save-deployment)
(define-generic load-deployment)
(define-generic list-deployments)

;; Generic methods for policy management
(define-generic save-policy)
(define-generic load-policy)
(define-generic delete-policy)
(define-generic list-policies)

;; Generic methods for transaction management
(define-generic begin-transaction)
(define-generic commit-transaction)
(define-generic rollback-transaction)

;; Generic methods for resource locking
(define-generic lock-resource)
(define-generic unlock-resource)

;; Default implementations that should be overridden by concrete backends

(define-method (save-state (backend <storage-backend>) state-id state-data)
  "Save state data - must be implemented by concrete backend"
  (error "save-state not implemented for" (storage-backend-name backend)))

(define-method (load-state (backend <storage-backend>) state-id)
  "Load state data - must be implemented by concrete backend"
  (error "load-state not implemented for" (storage-backend-name backend)))

(define-method (delete-state (backend <storage-backend>) state-id)
  "Delete state data - must be implemented by concrete backend"
  (error "delete-state not implemented for" (storage-backend-name backend)))

(define-method (list-states (backend <storage-backend>) . args)
  "List states with optional filtering - must be implemented by concrete backend"
  (error "list-states not implemented for" (storage-backend-name backend)))

(define-method (save-secret (backend <storage-backend>) secret-id encrypted-data)
  "Save encrypted secret - must be implemented by concrete backend"
  (error "save-secret not implemented for" (storage-backend-name backend)))

(define-method (load-secret (backend <storage-backend>) secret-id)
  "Load encrypted secret - must be implemented by concrete backend"
  (error "load-secret not implemented for" (storage-backend-name backend)))

(define-method (delete-secret (backend <storage-backend>) secret-id)
  "Delete secret - must be implemented by concrete backend"
  (error "delete-secret not implemented for" (storage-backend-name backend)))

(define-method (list-secrets (backend <storage-backend>) . args)
  "List secrets with optional namespace filtering - must be implemented by concrete backend"
  (error "list-secrets not implemented for" (storage-backend-name backend)))

(define-method (save-deployment (backend <storage-backend>) deployment-id deployment-data)
  "Save deployment record - must be implemented by concrete backend"
  (error "save-deployment not implemented for" (storage-backend-name backend)))

(define-method (load-deployment (backend <storage-backend>) deployment-id)
  "Load deployment record - must be implemented by concrete backend"
  (error "load-deployment not implemented for" (storage-backend-name backend)))

(define-method (list-deployments (backend <storage-backend>) stack-id . args)
  "List deployments for a stack - must be implemented by concrete backend"
  (error "list-deployments not implemented for" (storage-backend-name backend)))

(define-method (save-policy (backend <storage-backend>) policy-id policy-data)
  "Save policy - must be implemented by concrete backend"
  (error "save-policy not implemented for" (storage-backend-name backend)))

(define-method (load-policy (backend <storage-backend>) policy-id)
  "Load policy - must be implemented by concrete backend"
  (error "load-policy not implemented for" (storage-backend-name backend)))

(define-method (delete-policy (backend <storage-backend>) policy-id)
  "Delete policy - must be implemented by concrete backend"
  (error "delete-policy not implemented for" (storage-backend-name backend)))

(define-method (list-policies (backend <storage-backend>) . args)
  "List policies with optional filtering - must be implemented by concrete backend"
  (error "list-policies not implemented for" (storage-backend-name backend)))

(define-method (begin-transaction (backend <storage-backend>))
  "Begin a transaction - optional, returns transaction ID"
  #f)  ; Default: no transaction support

(define-method (commit-transaction (backend <storage-backend>) transaction-id)
  "Commit a transaction - optional"
  #t)  ; Default: always succeed

(define-method (rollback-transaction (backend <storage-backend>) transaction-id)
  "Rollback a transaction - optional"
  #t)  ; Default: always succeed

(define-method (lock-resource (backend <storage-backend>) resource-id . args)
  "Acquire a lock on a resource - optional, returns lock token"
  #f)  ; Default: no locking support

(define-method (unlock-resource (backend <storage-backend>) resource-id lock-token)
  "Release a lock on a resource - optional"
  #t)  ; Default: always succeed