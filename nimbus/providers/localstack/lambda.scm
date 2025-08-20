;;; lambda.scm - Lambda function resource implementation
;;; Copyright (C) 2025 Nimbus Contributors

(define-module (nimbus providers localstack lambda)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (nimbus providers localstack provider)
  #:export (<lambda-function>
            lambda-function-name
            lambda-runtime
            lambda-handler
            lambda-code-uri
            lambda-memory-size
            lambda-timeout
            lambda-environment
            lambda-layers
            make-lambda-function
            create-function
            delete-function
            update-function
            invoke-function))

(define-class <lambda-function> ()
  (id #:init-keyword #:id
      #:init-value #f
      #:accessor lambda-id)
  (function-name #:init-keyword #:function-name
                 #:getter lambda-function-name)
  (runtime #:init-keyword #:runtime
           #:init-value "python3.9"
           #:getter lambda-runtime)
  (handler #:init-keyword #:handler
           #:getter lambda-handler)
  (code-uri #:init-keyword #:code-uri
            #:getter lambda-code-uri)
  (memory-size #:init-keyword #:memory-size
               #:init-value 128
               #:getter lambda-memory-size)
  (timeout #:init-keyword #:timeout
           #:init-value 3
           #:getter lambda-timeout)
  (environment #:init-keyword #:environment
               #:init-value (make-hash-table)
               #:getter lambda-environment)
  (layers #:init-keyword #:layers
          #:init-value '()
          #:getter lambda-layers)
  (tags #:init-keyword #:tags
        #:init-value (make-hash-table)
        #:getter lambda-tags)
  (arn #:init-value #f
       #:accessor lambda-arn)
  (invoke-arn #:init-value #f
              #:accessor lambda-invoke-arn))

(define (make-lambda-function name . args)
  "Create a new Lambda function resource"
  (let* ((runtime (or (assq-ref args 'runtime) "python3.9"))
         (handler (or (assq-ref args 'handler) "index.handler"))
         (code-uri (assq-ref args 'code-uri))
         (memory-size (or (assq-ref args 'memory-size) 128))
         (timeout (or (assq-ref args 'timeout) 3))
         (environment (or (assq-ref args 'environment) (make-hash-table)))
         (layers (or (assq-ref args 'layers) '()))
         (tags (or (assq-ref args 'tags) (make-hash-table))))
    (make <lambda-function>
          #:function-name name
          #:runtime runtime
          #:handler handler
          #:code-uri code-uri
          #:memory-size memory-size
          #:timeout timeout
          #:environment environment
          #:layers layers
          #:tags tags)))

(define-method (create-function (provider <localstack-provider>) (func <lambda-function>))
  "Create a Lambda function in LocalStack"
  (let ((function-name (lambda-function-name func))
        (region (localstack-region provider)))
    (format #t "Creating Lambda function: ~a~%" function-name)
    
    ;; Package code (stub - would zip directory in real implementation)
    (let ((code-zip (package-lambda-code (lambda-code-uri func))))
      
      ;; Create function
      (provider-request provider
                       "lambda"
                       "CreateFunction"
                       `((FunctionName . ,function-name)
                         (Runtime . ,(lambda-runtime func))
                         (Handler . ,(lambda-handler func))
                         (Code . ((ZipFile . ,code-zip)))
                         (MemorySize . ,(lambda-memory-size func))
                         (Timeout . ,(lambda-timeout func))
                         (Environment . ((Variables . ,(hash-map->list cons 
                                                                      (lambda-environment func)))))
                         (Layers . ,(lambda-layers func))
                         (Tags . ,(hash-map->list cons (lambda-tags func))))))
    
    ;; Update function metadata
    (set! (lambda-arn func)
          (format #f "arn:aws:lambda:~a:000000000000:function:~a" 
                  region function-name))
    (set! (lambda-invoke-arn func)
          (format #f "arn:aws:apigateway:~a:lambda:path/2015-03-31/functions/~a/invocations"
                  region (lambda-arn func)))
    (set! (lambda-id func) function-name)
    
    func))

(define-method (delete-function (provider <localstack-provider>) (func <lambda-function>))
  "Delete a Lambda function"
  (let ((function-name (lambda-function-name func)))
    (format #t "Deleting Lambda function: ~a~%" function-name)
    
    (provider-request provider
                     "lambda"
                     "DeleteFunction"
                     `((FunctionName . ,function-name)))
    #t))

(define-method (update-function (provider <localstack-provider>) (func <lambda-function>) changes)
  "Update Lambda function configuration or code"
  (let ((function-name (lambda-function-name func)))
    (format #t "Updating Lambda function: ~a~%" function-name)
    
    ;; Update code if changed
    (when (assq-ref changes 'code-uri)
      (let ((code-zip (package-lambda-code (assq-ref changes 'code-uri))))
        (provider-request provider
                         "lambda"
                         "UpdateFunctionCode"
                         `((FunctionName . ,function-name)
                           (ZipFile . ,code-zip)))))
    
    ;; Update configuration if changed
    (when (or (assq-ref changes 'memory-size)
              (assq-ref changes 'timeout)
              (assq-ref changes 'environment))
      (provider-request provider
                       "lambda"
                       "UpdateFunctionConfiguration"
                       `((FunctionName . ,function-name)
                         ,@(if (assq-ref changes 'memory-size)
                               `((MemorySize . ,(assq-ref changes 'memory-size)))
                               '())
                         ,@(if (assq-ref changes 'timeout)
                               `((Timeout . ,(assq-ref changes 'timeout)))
                               '())
                         ,@(if (assq-ref changes 'environment)
                               `((Environment . ((Variables . ,(hash-map->list cons
                                                                             (assq-ref changes 'environment))))))
                               '()))))
    func))

(define-method (invoke-function (provider <localstack-provider>) (func <lambda-function>) payload)
  "Invoke a Lambda function"
  (let ((function-name (lambda-function-name func)))
    (format #t "Invoking Lambda function: ~a~%" function-name)
    
    (provider-request provider
                     "lambda"
                     "Invoke"
                     `((FunctionName . ,function-name)
                       (Payload . ,payload)))
    ;; Would return actual response
    '()))

(define (package-lambda-code directory)
  "Package Lambda code directory into a zip file (stub)"
  (format #t "Packaging Lambda code from: ~a~%" directory)
  ;; Would create actual zip file
  "BASE64_ENCODED_ZIP_CONTENT")