#!/usr/bin/env guile
!#
;;; test-secrets.scm - Unit tests for secrets management module

(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (nimbus models secrets)
             (oop goops)
             (ice-9 hash-table))

(test-begin "secrets-management")

;; Test encryption key creation
(test-group "encryption-key-creation"
  (let ((key (make-encryption-key 
               #:id "key-1"
               #:key-alias "master-key"
               #:key-material "dGVzdC1rZXktbWF0ZXJpYWwtZm9yLXRlc3Rpbmc=")))
    
    (test-equal "key-id" 
                "key-1" 
                (encryption-key-id key))
    
    (test-equal "key-alias" 
                "master-key" 
                (encryption-key-alias key))
    
    (test-equal "key-material" 
                "dGVzdC1rZXktbWF0ZXJpYWwtZm9yLXRlc3Rpbmc=" 
                (encryption-key-material key))
    
    (test-equal "key-algorithm" 
                "AES-256-CBC" 
                (encryption-key-algorithm key))
    
    (test-assert "key-is-active" 
                 (encryption-key-active? key))
    
    (test-assert "key-not-rotated" 
                 (not (encryption-key-rotated-at key)))))

;; Test secret creation
(test-group "secret-creation"
  (let ((secret (make-secret 
                  #:id "secret-1"
                  #:name "database-password"
                  #:namespace "production"
                  #:encrypted-value "encrypted-data-here"
                  #:encryption-key-id "key-1")))
    
    (test-equal "secret-id" 
                "secret-1" 
                (secret-id secret))
    
    (test-equal "secret-name" 
                "database-password" 
                (secret-name secret))
    
    (test-equal "secret-namespace" 
                "production" 
                (secret-namespace secret))
    
    (test-equal "secret-encrypted-value" 
                "encrypted-data-here" 
                (secret-encrypted-value secret))
    
    (test-equal "secret-encryption-key-id" 
                "key-1" 
                (secret-encryption-key-id secret))
    
    (test-equal "secret-version" 
                1 
                (secret-version secret))
    
    (test-assert "secret-metadata-is-hash-table" 
                 (hash-table? (secret-metadata secret)))))

;; Test secret access tracking
(test-group "secret-access"
  (let ((access (make-secret-access 
                  #:id "access-1"
                  #:secret-id "secret-1"
                  #:accessor-id "user-123"
                  #:access-type "read")))
    
    (test-equal "access-id" 
                "access-1" 
                (secret-access-id access))
    
    (test-equal "access-secret-id" 
                "secret-1" 
                (secret-access-secret-id access))
    
    (test-equal "access-accessor-id" 
                "user-123" 
                (secret-access-accessor-id access))
    
    (test-equal "access-type" 
                "read" 
                (secret-access-type access))
    
    (test-assert "access-timestamp-exists" 
                 (secret-access-accessed-at access))))

;; Test key rotation
(test-group "key-rotation"
  (let* ((original-key (make-encryption-key 
                         #:id "key-2"
                         #:key-alias "rotate-test"
                         #:key-material "b3JpZ2luYWwta2V5LW1hdGVyaWFs"))
         (new-material "bmV3LWtleS1tYXRlcmlhbC1hZnRlci1yb3RhdGlvbg==")
         (rotated-key (rotate-key original-key new-material)))
    
    (test-assert "original-key-inactive" 
                 (not (encryption-key-active? original-key)))
    
    (test-assert "original-key-has-rotation-date" 
                 (encryption-key-rotated-at original-key))
    
    (test-equal "rotated-key-id-updated" 
                "key-2-rotated" 
                (encryption-key-id rotated-key))
    
    (test-equal "rotated-key-same-alias" 
                "rotate-test" 
                (encryption-key-alias rotated-key))
    
    (test-equal "rotated-key-new-material" 
                new-material 
                (encryption-key-material rotated-key))
    
    (test-assert "rotated-key-is-active" 
                 (encryption-key-active? rotated-key))))

;; Test secret metadata
(test-group "secret-metadata"
  (let ((secret (make-secret 
                  #:id "secret-2"
                  #:name "api-key"
                  #:namespace "development"
                  #:encrypted-value "encrypted"
                  #:encryption-key-id "key-1")))
    
    ;; Add metadata
    (hash-set! (secret-metadata secret) "created-by" "admin")
    (hash-set! (secret-metadata secret) "environment" "dev")
    (hash-set! (secret-metadata secret) "rotation-interval" 90)
    
    (test-equal "metadata-created-by" 
                "admin" 
                (hash-ref (secret-metadata secret) "created-by"))
    
    (test-equal "metadata-environment" 
                "dev" 
                (hash-ref (secret-metadata secret) "environment"))
    
    (test-equal "metadata-rotation-interval" 
                90 
                (hash-ref (secret-metadata secret) "rotation-interval"))
    
    ;; Update version
    (set! (secret-version secret) 2)
    
    (test-equal "secret-version-updated" 
                2 
                (secret-version secret))))

;; Test multiple secrets in namespace
(test-group "namespace-management"
  (let ((secrets (list
                   (make-secret #:id "s1" #:name "db-pass" 
                               #:namespace "prod" 
                               #:encrypted-value "enc1"
                               #:encryption-key-id "k1")
                   (make-secret #:id "s2" #:name "api-key" 
                               #:namespace "prod" 
                               #:encrypted-value "enc2"
                               #:encryption-key-id "k1")
                   (make-secret #:id "s3" #:name "test-key" 
                               #:namespace "dev" 
                               #:encrypted-value "enc3"
                               #:encryption-key-id "k1"))))
    
    (let ((prod-secrets (filter (lambda (s) 
                                  (string=? (secret-namespace s) "prod"))
                                secrets)))
      
      (test-equal "two-secrets-in-prod-namespace" 
                  2 
                  (length prod-secrets))
      
      (test-assert "prod-secrets-have-correct-namespace" 
                   (every (lambda (s) 
                           (string=? (secret-namespace s) "prod"))
                         prod-secrets)))))

(test-end "secrets-management")