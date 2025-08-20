#!/usr/bin/env guile
!#
;;; test-deployment.scm - Unit tests for deployment history module

(use-modules (srfi srfi-64)
             (nimbus models deployment)
             (oop goops)
             (ice-9 hash-table))

(test-begin "deployment-history")

;; Test deployment status enumeration
(test-group "deployment-status"
  (test-assert "status-pending" 
               (eq? (deployment-status-value deployment-status-pending) 'pending))
  
  (test-assert "status-running" 
               (eq? (deployment-status-value deployment-status-running) 'running))
  
  (test-assert "status-completed" 
               (eq? (deployment-status-value deployment-status-completed) 'completed))
  
  (test-assert "status-failed" 
               (eq? (deployment-status-value deployment-status-failed) 'failed))
  
  (test-assert "status-rolled-back" 
               (eq? (deployment-status-value deployment-status-rolled-back) 'rolled-back)))

;; Test deployment type enumeration
(test-group "deployment-type"
  (test-assert "type-create" 
               (eq? (deployment-type-value deployment-type-create) 'create))
  
  (test-assert "type-update" 
               (eq? (deployment-type-value deployment-type-update) 'update))
  
  (test-assert "type-delete" 
               (eq? (deployment-type-value deployment-type-delete) 'delete))
  
  (test-assert "type-rollback" 
               (eq? (deployment-type-value deployment-type-rollback) 'rollback)))

;; Test deployment step creation
(test-group "deployment-step-creation"
  (let ((step (make-deployment-step 
                #:id "step-1"
                #:deployment-id "deployment-1"
                #:step-name "create-bucket"
                #:resource-id "bucket-1"
                #:action "create")))
    
    (test-equal "step-id" 
                "step-1" 
                (deployment-step-id step))
    
    (test-equal "step-deployment-id" 
                "deployment-1" 
                (deployment-step-deployment-id step))
    
    (test-equal "step-name" 
                "create-bucket" 
                (deployment-step-name step))
    
    (test-equal "step-resource-id" 
                "bucket-1" 
                (deployment-step-resource-id step))
    
    (test-equal "step-action" 
                "create" 
                (deployment-step-action step))
    
    (test-assert "step-status-is-pending" 
                 (eq? (deployment-status-value (deployment-step-status step)) 
                      'pending))
    
    (test-assert "step-not-started" 
                 (not (deployment-step-started-at step)))
    
    (test-assert "step-not-completed" 
                 (not (deployment-step-completed-at step)))))

;; Test deployment artifact creation
(test-group "deployment-artifact-creation"
  (let ((artifact (make-deployment-artifact 
                    #:id "artifact-1"
                    #:deployment-id "deployment-1"
                    #:artifact-type "plan"
                    #:artifact-uri "s3://bucket/plans/deployment-1.json"
                    #:checksum "abc123def456"
                    #:size-bytes 1024)))
    
    (test-equal "artifact-id" 
                "artifact-1" 
                (deployment-artifact-id artifact))
    
    (test-equal "artifact-deployment-id" 
                "deployment-1" 
                (deployment-artifact-deployment-id artifact))
    
    (test-equal "artifact-type" 
                "plan" 
                (deployment-artifact-type artifact))
    
    (test-equal "artifact-uri" 
                "s3://bucket/plans/deployment-1.json" 
                (deployment-artifact-uri artifact))
    
    (test-equal "artifact-checksum" 
                "abc123def456" 
                (deployment-artifact-checksum artifact))
    
    (test-equal "artifact-size" 
                1024 
                (deployment-artifact-size-bytes artifact))))

;; Test deployment creation
(test-group "deployment-creation"
  (let ((deployment (make-deployment 
                      #:id "deployment-1"
                      #:stack-id "stack-1"
                      #:environment "production"
                      #:deployer-id "user-123"
                      #:deployment-type deployment-type-create)))
    
    (test-equal "deployment-id" 
                "deployment-1" 
                (deployment-id deployment))
    
    (test-equal "deployment-stack-id" 
                "stack-1" 
                (deployment-stack-id deployment))
    
    (test-equal "deployment-environment" 
                "production" 
                (deployment-environment deployment))
    
    (test-equal "deployment-deployer-id" 
                "user-123" 
                (deployment-deployer-id deployment))
    
    (test-assert "deployment-type-is-create" 
                 (eq? (deployment-type-value (deployment-type deployment)) 
                      'create))
    
    (test-assert "deployment-status-is-pending" 
                 (eq? (deployment-status-value (deployment-status deployment)) 
                      'pending))
    
    (test-assert "deployment-not-completed" 
                 (not (deployment-completed-at deployment)))
    
    (test-assert "deployment-no-error" 
                 (not (deployment-error-message deployment)))))

;; Test deployment step management
(test-group "deployment-step-management"
  (let* ((deployment (make-deployment 
                       #:id "deployment-2"
                       #:stack-id "stack-2"
                       #:environment "staging"
                       #:deployer-id "user-456"
                       #:deployment-type deployment-type-update))
         (step1 (make-deployment-step 
                 #:id "step-1"
                 #:deployment-id "deployment-2"
                 #:step-name "update-instance"
                 #:resource-id "instance-1"
                 #:action "update"))
         (step2 (make-deployment-step 
                 #:id "step-2"
                 #:deployment-id "deployment-2"
                 #:step-name "update-database"
                 #:resource-id "db-1"
                 #:action "update")))
    
    ;; Add steps
    (add-step deployment step1)
    (add-step deployment step2)
    
    (test-equal "deployment-has-two-steps" 
                2 
                (length (deployment-steps deployment)))
    
    ;; Start first step
    (start-step step1)
    
    (test-assert "step1-is-running" 
                 (eq? (deployment-status-value (deployment-step-status step1)) 
                      'running))
    
    (test-assert "step1-has-start-time" 
                 (deployment-step-started-at step1))
    
    ;; Complete first step
    (let ((output (alist->hash-table '(("result" . "success")))))
      (complete-step step1 #:output-data output)
      
      (test-assert "step1-is-completed" 
                   (eq? (deployment-status-value (deployment-step-status step1)) 
                        'completed))
      
      (test-assert "step1-has-completion-time" 
                   (deployment-step-completed-at step1))
      
      (test-equal "step1-output-data" 
                  "success" 
                  (hash-ref (deployment-step-output-data step1) "result")))
    
    ;; Fail second step
    (start-step step2)
    (fail-step step2 "Database connection failed")
    
    (test-assert "step2-is-failed" 
                 (eq? (deployment-status-value (deployment-step-status step2)) 
                      'failed))
    
    (test-equal "step2-error-message" 
                "Database connection failed" 
                (deployment-step-error-message step2))))

;; Test deployment completion
(test-group "deployment-completion"
  (let ((deployment (make-deployment 
                      #:id "deployment-3"
                      #:stack-id "stack-3"
                      #:environment "development"
                      #:deployer-id "user-789"
                      #:deployment-type deployment-type-create)))
    
    ;; Complete successfully
    (complete-deployment deployment deployment-status-completed)
    
    (test-assert "deployment-is-completed" 
                 (eq? (deployment-status-value (deployment-status deployment)) 
                      'completed))
    
    (test-assert "deployment-has-completion-time" 
                 (deployment-completed-at deployment))
    
    (test-assert "deployment-no-error-on-success" 
                 (not (deployment-error-message deployment))))
  
  (let ((deployment (make-deployment 
                      #:id "deployment-4"
                      #:stack-id "stack-4"
                      #:environment "test"
                      #:deployer-id "user-000"
                      #:deployment-type deployment-type-delete)))
    
    ;; Complete with failure
    (complete-deployment deployment 
                        deployment-status-failed 
                        #:error-message "Resource not found")
    
    (test-assert "deployment-is-failed" 
                 (eq? (deployment-status-value (deployment-status deployment)) 
                      'failed))
    
    (test-equal "deployment-error-message" 
                "Resource not found" 
                (deployment-error-message deployment))))

;; Test deployment artifacts
(test-group "deployment-artifacts"
  (let* ((deployment (make-deployment 
                       #:id "deployment-5"
                       #:stack-id "stack-5"
                       #:environment "prod"
                       #:deployer-id "user-111"
                       #:deployment-type deployment-type-create))
         (artifact1 (make-deployment-artifact 
                     #:id "artifact-1"
                     #:deployment-id "deployment-5"
                     #:artifact-type "plan"
                     #:artifact-uri "s3://bucket/plan.json"
                     #:checksum "hash1"
                     #:size-bytes 512))
         (artifact2 (make-deployment-artifact 
                     #:id "artifact-2"
                     #:deployment-id "deployment-5"
                     #:artifact-type "state_backup"
                     #:artifact-uri "s3://bucket/backup.json"
                     #:checksum "hash2"
                     #:size-bytes 1024)))
    
    ;; Add artifacts
    (add-artifact deployment artifact1)
    (add-artifact deployment artifact2)
    
    (test-equal "deployment-has-two-artifacts" 
                2 
                (length (deployment-artifacts deployment)))))

;; Test get-steps-by-status
(test-group "steps-by-status"
  (let* ((deployment (make-deployment 
                       #:id "deployment-6"
                       #:stack-id "stack-6"
                       #:environment "staging"
                       #:deployer-id "user-222"
                       #:deployment-type deployment-type-update))
         (completed-step (make-deployment-step 
                          #:id "s1"
                          #:deployment-id "deployment-6"
                          #:step-name "step1"
                          #:resource-id "r1"
                          #:action "create"))
         (failed-step (make-deployment-step 
                       #:id "s2"
                       #:deployment-id "deployment-6"
                       #:step-name "step2"
                       #:resource-id "r2"
                       #:action "create"))
         (pending-step (make-deployment-step 
                        #:id "s3"
                        #:deployment-id "deployment-6"
                        #:step-name "step3"
                        #:resource-id "r3"
                        #:action "create")))
    
    ;; Setup steps with different statuses
    (set! (deployment-step-status completed-step) deployment-status-completed)
    (set! (deployment-step-status failed-step) deployment-status-failed)
    
    (add-step deployment completed-step)
    (add-step deployment failed-step)
    (add-step deployment pending-step)
    
    (test-equal "one-completed-step" 
                1 
                (length (get-steps-by-status deployment 
                                           deployment-status-completed)))
    
    (test-equal "one-failed-step" 
                1 
                (length (get-steps-by-status deployment 
                                           deployment-status-failed)))
    
    (test-equal "one-pending-step" 
                1 
                (length (get-steps-by-status deployment 
                                           deployment-status-pending)))))

;; Test deployment-in-progress?
(test-group "deployment-progress-check"
  (let ((running-deployment (make-deployment 
                              #:id "d1"
                              #:stack-id "s1"
                              #:environment "test"
                              #:deployer-id "u1"
                              #:deployment-type deployment-type-create))
        (completed-deployment (make-deployment 
                                #:id "d2"
                                #:stack-id "s2"
                                #:environment "test"
                                #:deployer-id "u2"
                                #:deployment-type deployment-type-create)))
    
    (set! (deployment-status running-deployment) deployment-status-running)
    (set! (deployment-status completed-deployment) deployment-status-completed)
    
    (test-assert "running-deployment-in-progress" 
                 (deployment-in-progress? running-deployment))
    
    (test-assert "completed-deployment-not-in-progress" 
                 (not (deployment-in-progress? completed-deployment)))))

(test-end "deployment-history")