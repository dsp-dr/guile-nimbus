;;; plan.scm - Execution plan calculation
;;; Copyright (C) 2025 Nimbus Contributors

(define-module (nimbus core plan)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (nimbus models state)
  #:export (<execution-plan>
            make-execution-plan
            plan-actions
            plan-additions
            plan-changes
            plan-deletions
            add-plan-action
            calculate-plan
            execute-plan
            save-plan
            load-plan))

(define-class <execution-plan> ()
  (actions #:init-keyword #:actions
           #:init-value '()
           #:accessor plan-actions)
  (additions #:init-keyword #:additions
             #:init-value 0
             #:accessor plan-additions)
  (changes #:init-keyword #:changes
           #:init-value 0
           #:accessor plan-changes)
  (deletions #:init-keyword #:deletions
             #:init-value 0
             #:accessor plan-deletions)
  (metadata #:init-keyword #:metadata
            #:init-value (make-hash-table)
            #:accessor plan-metadata))

(define (make-execution-plan)
  "Create a new execution plan"
  (make <execution-plan>))

(define-method (add-plan-action (plan <execution-plan>) action-type resource-type resource-id . details)
  "Add an action to the execution plan"
  (let ((action `(,action-type ,resource-type ,resource-id ,@details)))
    (set! (plan-actions plan) (append (plan-actions plan) (list action)))
    
    ;; Update counters
    (case action-type
      ((create) (set! (plan-additions plan) (+ 1 (plan-additions plan))))
      ((update) (set! (plan-changes plan) (+ 1 (plan-changes plan))))
      ((delete) (set! (plan-deletions plan) (+ 1 (plan-deletions plan))))))
  plan)

(define (calculate-plan current-state desired-config)
  "Calculate the execution plan based on current state and desired configuration"
  (let ((plan (make-execution-plan))
        (current-resources (state-resources current-state))
        (desired-resources (parse-desired-resources desired-config)))
    
    ;; Find resources to create (in desired but not in current)
    (hash-for-each
     (lambda (id resource)
       (unless (hash-ref current-resources id)
         (add-plan-action plan 'create 
                         (resource-type resource)
                         id
                         resource)))
     desired-resources)
    
    ;; Find resources to update (in both but different)
    (hash-for-each
     (lambda (id current-resource)
       (let ((desired-resource (hash-ref desired-resources id)))
         (when (and desired-resource
                   (not (resources-equal? current-resource desired-resource)))
           (add-plan-action plan 'update
                           (resource-type current-resource)
                           id
                           current-resource
                           desired-resource))))
     current-resources)
    
    ;; Find resources to delete (in current but not in desired)
    (hash-for-each
     (lambda (id resource)
       (unless (hash-ref desired-resources id)
         (add-plan-action plan 'delete
                         (resource-type resource)
                         id
                         resource)))
     current-resources)
    
    plan))

(define (parse-desired-resources config)
  "Parse desired resources from configuration"
  ;; Stub implementation - would parse infrastructure.nim
  (let ((resources (make-hash-table)))
    ;; Example resources
    (hash-set! resources "s3.data-bucket"
              `((type . "s3-bucket")
                (properties . ((bucket-name . "my-data-bucket")
                             (versioning . #t)
                             (encryption . "AES256")))))
    (hash-set! resources "lambda.processor"
              `((type . "lambda-function")
                (properties . ((function-name . "data-processor")
                             (runtime . "python3.9")
                             (handler . "main.handler")))))
    resources))

(define (resource-type resource)
  "Get resource type"
  (if (pair? resource)
      (assq-ref resource 'type)
      "unknown"))

(define (resources-equal? r1 r2)
  "Check if two resources are equal"
  ;; Simplified comparison
  (equal? (assq-ref r1 'properties)
          (assq-ref r2 'properties)))

(define (execute-plan plan provider)
  "Execute the plan using the provider"
  (for-each
   (lambda (action)
     (let ((action-type (car action))
           (resource-type (cadr action))
           (resource-id (caddr action)))
       (format #t "Executing: ~a ~a ~a~%" action-type resource-type resource-id)
       
       (case action-type
         ((create)
          (execute-create provider resource-type resource-id (cdddr action)))
         ((update)
          (execute-update provider resource-type resource-id (cdddr action)))
         ((delete)
          (execute-delete provider resource-type resource-id (cdddr action))))))
   (plan-actions plan)))

(define (execute-create provider resource-type resource-id details)
  "Execute resource creation"
  ;; Stub - would call provider methods
  (format #t "  Creating ~a: ~a~%" resource-type resource-id))

(define (execute-update provider resource-type resource-id details)
  "Execute resource update"
  (format #t "  Updating ~a: ~a~%" resource-type resource-id))

(define (execute-delete provider resource-type resource-id details)
  "Execute resource deletion"
  (format #t "  Deleting ~a: ~a~%" resource-type resource-id))

(define (save-plan plan filename)
  "Save execution plan to file"
  (call-with-output-file filename
    (lambda (port)
      (display ";; Nimbus execution plan\n" port)
      (write `(plan
               (actions ,(plan-actions plan))
               (additions ,(plan-additions plan))
               (changes ,(plan-changes plan))
               (deletions ,(plan-deletions plan)))
            port))))

(define (load-plan filename)
  "Load execution plan from file"
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (let ((data (read port)))
            (if (eq? (car data) 'plan)
                (let ((plan (make-execution-plan)))
                  (set! (plan-actions plan) (cadr (assq 'actions (cdr data))))
                  (set! (plan-additions plan) (cadr (assq 'additions (cdr data))))
                  (set! (plan-changes plan) (cadr (assq 'changes (cdr data))))
                  (set! (plan-deletions plan) (cadr (assq 'deletions (cdr data))))
                  plan)
                (make-execution-plan)))))
      (make-execution-plan)))