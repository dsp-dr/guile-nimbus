#!/usr/bin/env guile
!#
;;; test-policy.scm - Unit tests for policy engine module

(use-modules (srfi srfi-64)
             (nimbus models policy)
             (oop goops)
             (ice-9 hash-table))

(test-begin "policy-engine")

;; Test policy effect enumeration
(test-group "policy-effects"
  (test-assert "effect-allow" 
               (eq? (policy-effect-value policy-effect-allow) 'allow))
  
  (test-assert "effect-deny" 
               (eq? (policy-effect-value policy-effect-deny) 'deny)))

;; Test policy rule creation
(test-group "policy-rule-creation"
  (let ((rule (make-policy-rule 
                #:actions '("s3:GetObject" "s3:ListBucket")
                #:resources '("arn:aws:s3:::my-bucket/*")
                #:conditions #f)))
    
    (test-equal "rule-actions" 
                '("s3:GetObject" "s3:ListBucket")
                (policy-rule-actions rule))
    
    (test-equal "rule-resources" 
                '("arn:aws:s3:::my-bucket/*")
                (policy-rule-resources rule))
    
    (test-assert "rule-no-conditions" 
                 (not (policy-rule-conditions rule)))))

;; Test policy creation
(test-group "policy-creation"
  (let ((policy (make-policy 
                  #:id "policy-1"
                  #:name "read-only-policy"
                  #:description "Allows read-only access"
                  #:effect policy-effect-allow)))
    
    (test-equal "policy-id" 
                "policy-1" 
                (policy-id policy))
    
    (test-equal "policy-name" 
                "read-only-policy" 
                (policy-name policy))
    
    (test-equal "policy-description" 
                "Allows read-only access" 
                (policy-description policy))
    
    (test-assert "policy-effect-is-allow" 
                 (eq? (policy-effect-value (policy-effect policy)) 'allow))
    
    (test-assert "policy-is-active" 
                 (policy-active? policy))
    
    (test-assert "policy-rules-empty" 
                 (null? (policy-rules policy)))))

;; Test adding rules to policy
(test-group "policy-rule-management"
  (let* ((policy (make-policy 
                   #:id "policy-2"
                   #:name "test-policy"
                   #:description "Policy with rules"))
         (rule1 (make-policy-rule 
                 #:actions '("ec2:*")
                 #:resources '("*")))
         (rule2 (make-policy-rule 
                 #:actions '("s3:GetObject")
                 #:resources '("arn:aws:s3:::bucket/*"))))
    
    ;; Add rules
    (add-rule policy rule1)
    (add-rule policy rule2)
    
    (test-equal "policy-has-two-rules" 
                2 
                (length (policy-rules policy)))
    
    ;; Remove a rule
    (remove-rule policy 0)
    
    (test-equal "policy-has-one-rule" 
                1 
                (length (policy-rules policy)))
    
    (test-equal "remaining-rule-is-rule2" 
                rule2 
                (car (policy-rules policy)))))

;; Test policy evaluation
(test-group "policy-evaluation"
  (let* ((allow-policy (make-policy 
                         #:id "allow-policy"
                         #:name "allow-s3"
                         #:description "Allow S3 access"
                         #:effect policy-effect-allow))
         (deny-policy (make-policy 
                        #:id "deny-policy"
                        #:name "deny-ec2"
                        #:description "Deny EC2 access"
                        #:effect policy-effect-deny))
         (s3-rule (make-policy-rule 
                   #:actions '("s3:*")
                   #:resources '("arn:aws:s3:::*")))
         (ec2-rule (make-policy-rule 
                    #:actions '("ec2:*")
                    #:resources '("*")))
         (context (make-hash-table)))
    
    ;; Setup policies
    (add-rule allow-policy s3-rule)
    (add-rule deny-policy ec2-rule)
    
    ;; Test allow policy
    (test-assert "allow-s3-getobject" 
                 (evaluate-policy allow-policy 
                                 "s3:GetObject" 
                                 "arn:aws:s3:::my-bucket/file.txt"
                                 context))
    
    (test-assert "allow-s3-putobject" 
                 (evaluate-policy allow-policy 
                                 "s3:PutObject" 
                                 "arn:aws:s3:::my-bucket/file.txt"
                                 context))
    
    (test-assert "not-allow-ec2-action" 
                 (not (evaluate-policy allow-policy 
                                      "ec2:DescribeInstances" 
                                      "arn:aws:ec2:::instance/*"
                                      context)))
    
    ;; Test deny policy
    (test-assert "deny-ec2-action" 
                 (not (evaluate-policy deny-policy 
                                      "ec2:TerminateInstances" 
                                      "*"
                                      context)))
    
    (test-assert "not-deny-s3-action" 
                 (evaluate-policy deny-policy 
                                 "s3:GetObject" 
                                 "arn:aws:s3:::bucket/file"
                                 context))))

;; Test wildcard pattern matching
(test-group "pattern-matching"
  (let* ((policy (make-policy 
                   #:id "pattern-policy"
                   #:name "pattern-test"
                   #:description "Test pattern matching"
                   #:effect policy-effect-allow))
         (rule (make-policy-rule 
                 #:actions '("s3:Get*" "s3:List*")
                 #:resources '("arn:aws:s3:::my-*/*")))
         (context (make-hash-table)))
    
    (add-rule policy rule)
    
    ;; Matching patterns
    (test-assert "matches-get-action" 
                 (evaluate-policy policy 
                                 "s3:GetObject" 
                                 "arn:aws:s3:::my-bucket/file.txt"
                                 context))
    
    (test-assert "matches-list-action" 
                 (evaluate-policy policy 
                                 "s3:ListBucket" 
                                 "arn:aws:s3:::my-data/dir/"
                                 context))
    
    ;; Non-matching patterns
    (test-assert "not-matches-put-action" 
                 (not (evaluate-policy policy 
                                      "s3:PutObject" 
                                      "arn:aws:s3:::my-bucket/file.txt"
                                      context)))
    
    (test-assert "not-matches-other-bucket" 
                 (not (evaluate-policy policy 
                                      "s3:GetObject" 
                                      "arn:aws:s3:::other-bucket/file.txt"
                                      context)))))

;; Test policy binding
(test-group "policy-binding"
  (let ((binding (make-policy-binding 
                   #:id "binding-1"
                   #:policy-id "policy-1"
                   #:subject-type "user"
                   #:subject-id "user-123"
                   #:resource-pattern "arn:aws:s3:::*")))
    
    (test-equal "binding-id" 
                "binding-1" 
                (policy-binding-id binding))
    
    (test-equal "binding-policy-id" 
                "policy-1" 
                (policy-binding-policy-id binding))
    
    (test-equal "binding-subject-type" 
                "user" 
                (policy-binding-subject-type binding))
    
    (test-equal "binding-subject-id" 
                "user-123" 
                (policy-binding-subject-id binding))
    
    (test-equal "binding-resource-pattern" 
                "arn:aws:s3:::*" 
                (policy-binding-resource-pattern binding))))

;; Test policy evaluation record
(test-group "policy-evaluation-record"
  (let ((eval-record (make-policy-evaluation 
                       #:id "eval-1"
                       #:policy-id "policy-1"
                       #:action "s3:GetObject"
                       #:resource "arn:aws:s3:::bucket/file"
                       #:subject "user-123"
                       #:allowed #t)))
    
    (test-equal "eval-id" 
                "eval-1" 
                (policy-evaluation-id eval-record))
    
    (test-equal "eval-policy-id" 
                "policy-1" 
                (policy-evaluation-policy-id eval-record))
    
    (test-equal "eval-action" 
                "s3:GetObject" 
                (policy-evaluation-action eval-record))
    
    (test-equal "eval-resource" 
                "arn:aws:s3:::bucket/file" 
                (policy-evaluation-resource eval-record))
    
    (test-equal "eval-subject" 
                "user-123" 
                (policy-evaluation-subject eval-record))
    
    (test-assert "eval-allowed" 
                 (policy-evaluation-allowed? eval-record))))

;; Test conditional policies
(test-group "conditional-policies"
  (let* ((policy (make-policy 
                   #:id "conditional-policy"
                   #:name "ip-restricted"
                   #:description "Allow only from specific IP"
                   #:effect policy-effect-allow))
         (rule (make-policy-rule 
                 #:actions '("*")
                 #:resources '("*")
                 #:conditions (alist->hash-table 
                              '(("source-ip" . "192.168.1.1")))))
         (context-allowed (alist->hash-table 
                          '(("source-ip" . "192.168.1.1"))))
         (context-denied (alist->hash-table 
                         '(("source-ip" . "10.0.0.1")))))
    
    (add-rule policy rule)
    
    (test-assert "allow-from-correct-ip" 
                 (evaluate-policy policy 
                                 "s3:GetObject" 
                                 "arn:aws:s3:::bucket/file"
                                 context-allowed))
    
    (test-assert "deny-from-wrong-ip" 
                 (not (evaluate-policy policy 
                                      "s3:GetObject" 
                                      "arn:aws:s3:::bucket/file"
                                      context-denied)))))

(test-end "policy-engine")