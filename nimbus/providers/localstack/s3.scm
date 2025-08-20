;;; s3.scm - S3 bucket resource implementation
;;; Copyright (C) 2025 Nimbus Contributors

(define-module (nimbus providers localstack s3)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (nimbus providers localstack provider)
  #:export (<s3-bucket>
            s3-bucket-name
            s3-bucket-versioning
            s3-bucket-encryption
            s3-bucket-lifecycle-rules
            s3-bucket-tags
            make-s3-bucket
            create-bucket
            delete-bucket
            update-bucket
            read-bucket
            list-buckets))

(define-class <s3-bucket> ()
  (id #:init-keyword #:id
      #:init-value #f
      #:accessor s3-bucket-id)
  (bucket-name #:init-keyword #:bucket-name
               #:getter s3-bucket-name)
  (versioning #:init-keyword #:versioning
              #:init-value #f
              #:getter s3-bucket-versioning)
  (encryption #:init-keyword #:encryption
              #:init-value "AES256"
              #:getter s3-bucket-encryption)
  (lifecycle-rules #:init-keyword #:lifecycle-rules
                   #:init-value '()
                   #:getter s3-bucket-lifecycle-rules)
  (tags #:init-keyword #:tags
        #:init-value (make-hash-table)
        #:getter s3-bucket-tags)
  (arn #:init-value #f
       #:accessor s3-bucket-arn)
  (created-at #:init-value #f
              #:accessor s3-bucket-created-at))

(define (make-s3-bucket name . args)
  "Create a new S3 bucket resource"
  (let* ((versioning (assq-ref args 'versioning))
         (encryption (or (assq-ref args 'encryption) "AES256"))
         (lifecycle-rules (or (assq-ref args 'lifecycle-rules) '()))
         (tags (or (assq-ref args 'tags) (make-hash-table))))
    (make <s3-bucket>
          #:bucket-name name
          #:versioning versioning
          #:encryption encryption
          #:lifecycle-rules lifecycle-rules
          #:tags tags)))

(define-method (create-bucket (provider <localstack-provider>) (bucket <s3-bucket>))
  "Create an S3 bucket in LocalStack"
  (let ((bucket-name (s3-bucket-name bucket))
        (region (localstack-region provider)))
    (format #t "Creating S3 bucket: ~a in ~a~%" bucket-name region)
    
    ;; Make API call to create bucket
    (provider-request provider 
                     "s3" 
                     "CreateBucket"
                     `((Bucket . ,bucket-name)
                       (CreateBucketConfiguration . ((LocationConstraint . ,region)))))
    
    ;; Enable versioning if requested
    (when (s3-bucket-versioning bucket)
      (provider-request provider
                       "s3"
                       "PutBucketVersioning"
                       `((Bucket . ,bucket-name)
                         (VersioningConfiguration . ((Status . "Enabled"))))))
    
    ;; Set encryption
    (when (s3-bucket-encryption bucket)
      (provider-request provider
                       "s3"
                       "PutBucketEncryption"
                       `((Bucket . ,bucket-name)
                         (ServerSideEncryptionConfiguration . 
                          ((Rules . ((ApplyServerSideEncryptionByDefault . 
                                     ((SSEAlgorithm . ,(s3-bucket-encryption bucket))))))))))
    
    ;; Set lifecycle rules
    (unless (null? (s3-bucket-lifecycle-rules bucket))
      (provider-request provider
                       "s3"
                       "PutBucketLifecycleConfiguration"
                       `((Bucket . ,bucket-name)
                         (Rules . ,(s3-bucket-lifecycle-rules bucket)))))
    
    ;; Set tags
    (unless (zero? (hash-count (const #t) (s3-bucket-tags bucket)))
      (let ((tag-set (hash-map->list cons (s3-bucket-tags bucket))))
        (provider-request provider
                         "s3"
                         "PutBucketTagging"
                         `((Bucket . ,bucket-name)
                           (Tagging . ((TagSet . ,tag-set)))))))
    
    ;; Update bucket metadata
    (set! (s3-bucket-arn bucket) 
          (format #f "arn:aws:s3:::~a" bucket-name))
    (set! (s3-bucket-created-at bucket) 
          (current-time))
    (set! (s3-bucket-id bucket) bucket-name)
    
    bucket))

(define-method (delete-bucket (provider <localstack-provider>) (bucket <s3-bucket>))
  "Delete an S3 bucket"
  (let ((bucket-name (s3-bucket-name bucket)))
    (format #t "Deleting S3 bucket: ~a~%" bucket-name)
    
    ;; First delete all objects (would need ListObjectsV2 in real implementation)
    (provider-request provider
                     "s3"
                     "DeleteBucket"
                     `((Bucket . ,bucket-name)))
    #t))

(define-method (update-bucket (provider <localstack-provider>) (bucket <s3-bucket>) changes)
  "Update S3 bucket configuration"
  (let ((bucket-name (s3-bucket-name bucket)))
    (format #t "Updating S3 bucket: ~a~%" bucket-name)
    
    ;; Update versioning if changed
    (when (assq-ref changes 'versioning)
      (provider-request provider
                       "s3"
                       "PutBucketVersioning"
                       `((Bucket . ,bucket-name)
                         (VersioningConfiguration . 
                          ((Status . ,(if (assq-ref changes 'versioning)
                                        "Enabled"
                                        "Suspended")))))))
    
    ;; Update other properties as needed
    bucket))

(define-method (read-bucket (provider <localstack-provider>) bucket-name)
  "Read S3 bucket configuration from LocalStack"
  (format #t "Reading S3 bucket: ~a~%" bucket-name)
  
  ;; Would make actual API calls to get bucket configuration
  (let ((bucket (make-s3-bucket bucket-name)))
    (set! (s3-bucket-arn bucket) 
          (format #f "arn:aws:s3:::~a" bucket-name))
    bucket))

(define-method (list-buckets (provider <localstack-provider>))
  "List all S3 buckets"
  (format #t "Listing S3 buckets~%")
  
  ;; Would make actual API call
  (provider-request provider "s3" "ListBuckets")
  '())) ;; Return empty list for now