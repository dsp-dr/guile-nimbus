#!/usr/bin/env guile
!#
;;; test-storage.scm - Unit tests for storage interface module

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (nimbus storage interface)
             (oop goops)
             (ice-9 hash-table))

(test-begin "storage-interface")

;; Mock storage backend for testing
(define-class <mock-storage-backend> (<storage-backend>)
  (data-store #:init-form (make-hash-table)
              #:accessor mock-data-store))

;; Implement mock methods
(define-method (save-state (backend <mock-storage-backend>) state-id state-data)
  (hash-set! (mock-data-store backend) 
             (string-append "state:" state-id) 
             state-data))

(define-method (load-state (backend <mock-storage-backend>) state-id)
  (hash-ref (mock-data-store backend) 
            (string-append "state:" state-id)))

(define-method (delete-state (backend <mock-storage-backend>) state-id)
  (hash-remove! (mock-data-store backend) 
                (string-append "state:" state-id)))

(define-method (list-states (backend <mock-storage-backend>) . args)
  (let ((all-keys (hash-map->list (lambda (k v) k) (mock-data-store backend))))
    (filter (lambda (key) (string-prefix? "state:" key)) all-keys)))

(define-method (save-secret (backend <mock-storage-backend>) secret-id encrypted-data)
  (hash-set! (mock-data-store backend) 
             (string-append "secret:" secret-id) 
             encrypted-data))

(define-method (load-secret (backend <mock-storage-backend>) secret-id)
  (hash-ref (mock-data-store backend) 
            (string-append "secret:" secret-id)))

(define-method (save-deployment (backend <mock-storage-backend>) deployment-id deployment-data)
  (hash-set! (mock-data-store backend) 
             (string-append "deployment:" deployment-id) 
             deployment-data))

(define-method (list-deployments (backend <mock-storage-backend>) stack-id . args)
  (let* ((limit (if (and (pair? args) (pair? (cdr args)) (eq? (car args) #:limit))
                    (cadr args)
                    10))
         (all-keys (hash-map->list (lambda (k v) k) (mock-data-store backend)))
         (deployment-keys (filter (lambda (key) (string-prefix? "deployment:" key)) all-keys)))
    (take deployment-keys (min limit (length deployment-keys)))))

;; Test storage backend creation
(test-group "storage-backend-creation"
  (let* ((config (alist->hash-table '(("host" . "localhost") 
                                      ("port" . 5432))))
         (backend (make <mock-storage-backend> 
                    #:name "mock-backend"
                    #:config config)))
    
    (test-equal "backend-name" 
                "mock-backend" 
                (storage-backend-name backend))
    
    (test-assert "backend-config-is-hash-table" 
                 (hash-table? (storage-backend-config backend)))
    
    (test-equal "backend-config-host" 
                "localhost" 
                (hash-ref (storage-backend-config backend) "host"))))

;; Test state storage operations
(test-group "state-storage"
  (let ((backend (make <mock-storage-backend> #:name "test-backend")))
    
    ;; Save state
    (let ((state-data (alist->hash-table 
                       '(("stack-name" . "prod-stack")
                         ("environment" . "production")
                         ("version" . 1)))))
      (save-state backend "state-1" state-data)
      
      ;; Load state
      (let ((loaded-state (load-state backend "state-1")))
        (test-assert "state-loaded" 
                     (hash-table? loaded-state))
        
        (test-equal "state-stack-name" 
                    "prod-stack" 
                    (hash-ref loaded-state "stack-name"))
        
        (test-equal "state-environment" 
                    "production" 
                    (hash-ref loaded-state "environment")))
      
      ;; List states
      (save-state backend "state-2" state-data)
      (let ((states (list-states backend)))
        (test-equal "two-states-listed" 
                    2 
                    (length states)))
      
      ;; Delete state
      (delete-state backend "state-1")
      (test-assert "state-deleted" 
                   (not (load-state backend "state-1"))))))

;; Test secret storage operations
(test-group "secret-storage"
  (let ((backend (make <mock-storage-backend> #:name "test-backend")))
    
    ;; Save secret
    (save-secret backend "secret-1" "encrypted-password-data")
    
    ;; Load secret
    (let ((loaded-secret (load-secret backend "secret-1")))
      (test-equal "secret-loaded" 
                  "encrypted-password-data" 
                  loaded-secret))
    
    ;; Save multiple secrets
    (save-secret backend "secret-2" "encrypted-api-key")
    (save-secret backend "secret-3" "encrypted-token")
    
    (test-equal "secret-2-loaded" 
                "encrypted-api-key" 
                (load-secret backend "secret-2"))
    
    (test-equal "secret-3-loaded" 
                "encrypted-token" 
                (load-secret backend "secret-3"))))

;; Test deployment storage operations
(test-group "deployment-storage"
  (let ((backend (make <mock-storage-backend> #:name "test-backend")))
    
    ;; Save deployment
    (let ((deployment-data (alist->hash-table 
                           '(("stack-id" . "stack-1")
                             ("environment" . "staging")
                             ("status" . "completed")))))
      (save-deployment backend "deployment-1" deployment-data)
      (save-deployment backend "deployment-2" deployment-data)
      (save-deployment backend "deployment-3" deployment-data)
      
      ;; List deployments
      (let ((deployments (list-deployments backend "stack-1" #:limit 2)))
        (test-assert "deployments-limited" 
                     (<= (length deployments) 2))))))

;; Test transaction support (optional)
(test-group "transaction-support"
  (let ((backend (make <mock-storage-backend> #:name "test-backend")))
    
    ;; Default implementation returns #f for no support
    (test-assert "begin-transaction-returns-false" 
                 (not (begin-transaction backend)))
    
    ;; Default implementation always succeeds
    (test-assert "commit-transaction-succeeds" 
                 (commit-transaction backend "tx-123"))
    
    (test-assert "rollback-transaction-succeeds" 
                 (rollback-transaction backend "tx-123"))))

;; Test resource locking (optional)
(test-group "resource-locking"
  (let ((backend (make <mock-storage-backend> #:name "test-backend")))
    
    ;; Default implementation returns #f for no support
    (test-assert "lock-resource-returns-false" 
                 (not (lock-resource backend "resource-1")))
    
    ;; Default implementation always succeeds
    (test-assert "unlock-resource-succeeds" 
                 (unlock-resource backend "resource-1" "lock-token-123"))))

;; Test error handling for unimplemented methods
(test-group "unimplemented-methods"
  (let ((backend (make <storage-backend> #:name "base-backend")))
    
    ;; These should throw errors when called on base class
    (test-error "save-state-not-implemented" 
                #t
                (save-state backend "state-1" (make-hash-table)))
    
    (test-error "load-state-not-implemented" 
                #t
                (load-state backend "state-1"))
    
    (test-error "save-policy-not-implemented" 
                #t
                (save-policy backend "policy-1" (make-hash-table)))
    
    (test-error "load-policy-not-implemented" 
                #t
                (load-policy backend "policy-1"))
    
    (test-error "delete-policy-not-implemented" 
                #t
                (delete-policy backend "policy-1"))
    
    (test-error "list-policies-not-implemented" 
                #t
                (list-policies backend))))

(test-end "storage-interface")