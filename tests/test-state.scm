#!/usr/bin/env guile
!#
;;; test-state.scm - Unit tests for state management module

(use-modules (srfi srfi-64)
             (nimbus models state)
             (oop goops)
             (ice-9 hash-table))

(test-begin "state-management")

;; Test resource creation
(test-group "resource-creation"
  (let ((resource (make-resource 
                    #:id "test-resource-1"
                    #:state-id "test-state-1"
                    #:resource-type "AWS::S3::Bucket"
                    #:resource-name "test-bucket")))
    
    (test-equal "resource-id" 
                "test-resource-1" 
                (resource-id resource))
    
    (test-equal "resource-state-id" 
                "test-state-1" 
                (resource-state-id resource))
    
    (test-equal "resource-type" 
                "AWS::S3::Bucket" 
                (resource-type resource))
    
    (test-equal "resource-name" 
                "test-bucket" 
                (resource-name resource))
    
    (test-assert "resource-status-is-pending" 
                 (eq? (resource-status-value (resource-status resource)) 
                      'pending))
    
    (test-assert "resource-dependencies-empty" 
                 (null? (resource-dependencies resource)))
    
    (test-assert "resource-properties-is-hash-table" 
                 (hash-table? (resource-properties resource)))))

;; Test state creation
(test-group "state-creation"
  (let ((state (make-state 
                 #:id "test-state-1"
                 #:stack-name "test-stack"
                 #:environment "test")))
    
    (test-equal "state-id" 
                "test-state-1" 
                (state-id state))
    
    (test-equal "state-stack-name" 
                "test-stack" 
                (state-stack-name state))
    
    (test-equal "state-environment" 
                "test" 
                (state-environment state))
    
    (test-equal "state-version" 
                1 
                (state-version state))
    
    (test-assert "state-resources-is-hash-table" 
                 (hash-table? (state-resources state)))
    
    (test-assert "state-outputs-is-hash-table" 
                 (hash-table? (state-outputs state)))))

;; Test adding resources to state
(test-group "state-resource-management"
  (let* ((state (make-state 
                  #:id "test-state-2"
                  #:stack-name "test-stack"
                  #:environment "test"))
         (resource1 (make-resource 
                     #:id "resource-1"
                     #:state-id "test-state-2"
                     #:resource-type "AWS::EC2::Instance"
                     #:resource-name "instance-1"))
         (resource2 (make-resource 
                     #:id "resource-2"
                     #:state-id "test-state-2"
                     #:resource-type "AWS::S3::Bucket"
                     #:resource-name "bucket-1")))
    
    ;; Add resources
    (add-resource state resource1)
    (add-resource state resource2)
    
    (test-equal "state-has-two-resources" 
                2 
                (hash-count (const #t) (state-resources state)))
    
    (test-equal "get-resource-by-id" 
                resource1 
                (get-resource state "resource-1"))
    
    (test-equal "state-version-incremented" 
                3 
                (state-version state))
    
    ;; Update resource
    (set! (resource-status resource1) resource-status-created)
    (update-resource state resource1)
    
    (test-assert "resource-updated" 
                 (eq? (resource-status-value 
                       (resource-status (get-resource state "resource-1")))
                      'created))
    
    ;; Remove resource
    (remove-resource state "resource-2")
    
    (test-equal "resource-removed" 
                1 
                (hash-count (const #t) (state-resources state)))
    
    (test-assert "removed-resource-not-found" 
                 (not (get-resource state "resource-2")))))

;; Test state snapshot creation
(test-group "state-snapshot"
  (let ((snapshot (make-state-snapshot 
                    #:id "snapshot-1"
                    #:state-id "test-state-1"
                    #:operation-type "create")))
    
    (test-equal "snapshot-id" 
                "snapshot-1" 
                (state-snapshot-id snapshot))
    
    (test-equal "snapshot-state-id" 
                "test-state-1" 
                (state-snapshot-state-id snapshot))
    
    (test-equal "snapshot-operation-type" 
                "create" 
                (state-snapshot-operation-type snapshot))
    
    (test-assert "snapshot-data-is-hash-table" 
                 (hash-table? (state-snapshot-data snapshot)))))

;; Test checksum calculation
(test-group "state-checksum"
  (let ((state (make-state 
                 #:id "test-state-3"
                 #:stack-name "test-stack"
                 #:environment "test")))
    
    ;; Add some data
    (hash-set! (state-outputs state) "output1" "value1")
    (hash-set! (state-outputs state) "output2" "value2")
    
    (let ((checksum1 (calculate-checksum state)))
      (test-assert "checksum-is-string" 
                   (string? checksum1))
      
      (test-assert "checksum-has-length" 
                   (> (string-length checksum1) 0))
      
      ;; Modify state and verify checksum changes
      (hash-set! (state-outputs state) "output3" "value3")
      
      (let ((checksum2 (calculate-checksum state)))
        (test-assert "checksum-changes-with-state" 
                     (not (string=? checksum1 checksum2)))))))

;; Test resource status enumeration
(test-group "resource-status"
  (test-assert "status-pending" 
               (eq? (resource-status-value resource-status-pending) 'pending))
  
  (test-assert "status-creating" 
               (eq? (resource-status-value resource-status-creating) 'creating))
  
  (test-assert "status-created" 
               (eq? (resource-status-value resource-status-created) 'created))
  
  (test-assert "status-updating" 
               (eq? (resource-status-value resource-status-updating) 'updating))
  
  (test-assert "status-deleting" 
               (eq? (resource-status-value resource-status-deleting) 'deleting))
  
  (test-assert "status-deleted" 
               (eq? (resource-status-value resource-status-deleted) 'deleted))
  
  (test-assert "status-failed" 
               (eq? (resource-status-value resource-status-failed) 'failed)))

(test-end "state-management")