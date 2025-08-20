;;; web-app/infrastructure.nim - Complete Web Application Example
;;; Demonstrates a real-world web application with frontend, backend, and storage

(define-infrastructure web-app
  ;; Configuration
  (provider localstack
    (endpoint (env-var "LOCALSTACK_ENDPOINT" "http://localhost:4566"))
    (region "us-east-1")
    (access-key "test")
    (secret-key "test"))

  ;; Variables for environment-specific configuration
  (variables
    (environment 
      (type string)
      (default "development")
      (description "Deployment environment"))
    (app-name
      (type string)
      (default "web-app")
      (description "Application name prefix"))
    (api-memory
      (type number)
      (default 256)
      (description "Lambda memory allocation"))
    (retention-days
      (type number)
      (default 7)
      (description "Log retention in days")))

  ;; S3 Bucket for storing uploaded files
  (resource s3-bucket "uploads"
    (bucket-name (format "~a-~a-uploads" (var app-name) (var environment)))
    (versioning #t)
    (cors-configuration
      (cors-rules
        ((allowed-headers '("*"))
         (allowed-methods '("GET" "PUT" "POST" "DELETE"))
         (allowed-origins '("*"))
         (max-age-seconds 3600))))
    (lifecycle-rules
      ((id "cleanup-old-uploads")
       (status "Enabled")
       (expiration-days 30)
       (noncurrent-version-expiration-days 7)))
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "storage")))

  ;; S3 Bucket for hosting static website
  (resource s3-bucket "website"
    (bucket-name (format "~a-~a-website" (var app-name) (var environment)))
    (website-configuration
      (index-document "index.html")
      (error-document "error.html"))
    (public-read-policy #t)
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "frontend")))

  ;; DynamoDB table for application data
  (resource dynamodb-table "users"
    (table-name (format "~a-~a-users" (var app-name) (var environment)))
    (hash-key "user_id" "S")
    (billing-mode "PAY_PER_REQUEST")
    (stream-specification
      (stream-enabled #t)
      (stream-view-type "NEW_AND_OLD_IMAGES"))
    (global-secondary-indexes
      ((name "email-index")
       (hash-key "email" "S")
       (projection-type "ALL"))
      ((name "created-index")
       (hash-key "created_at" "S")
       (projection-type "KEYS_ONLY")))
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "database")))

  ;; Lambda function for API backend
  (resource lambda-function "api"
    (function-name (format "~a-~a-api" (var app-name) (var environment)))
    (runtime "python3.9")
    (handler "app.handler")
    (code 
      (from-directory "./api/"))
    (memory-size (var api-memory))
    (timeout 30)
    (environment-variables
      (UPLOADS_BUCKET (ref s3-bucket "uploads" "name"))
      (USERS_TABLE (ref dynamodb-table "users" "name"))
      (ENVIRONMENT (var environment))
      (LOG_LEVEL "INFO"))
    (layers
      ("arn:aws:lambda:us-east-1:123456789:layer:requests:1"))
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "backend")))

  ;; Lambda function for processing uploaded files
  (resource lambda-function "file-processor"
    (function-name (format "~a-~a-processor" (var app-name) (var environment)))
    (runtime "python3.9")
    (handler "processor.handler")
    (code
      (from-directory "./processor/"))
    (memory-size 512)
    (timeout 300)  ; 5 minutes for file processing
    (environment-variables
      (UPLOADS_BUCKET (ref s3-bucket "uploads" "name"))
      (PROCESSED_BUCKET (ref s3-bucket "website" "name")))
    (s3-triggers
      ((bucket (ref s3-bucket "uploads" "name"))
       (events '("s3:ObjectCreated:*"))
       (filter-prefix "raw/")
       (filter-suffix ".jpg")))
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "processing")))

  ;; IAM Role for Lambda functions
  (resource iam-role "lambda-execution-role"
    (role-name (format "~a-~a-lambda-role" (var app-name) (var environment)))
    (assume-role-policy
      (version "2012-10-17")
      (statement
        ((effect "Allow")
         (principal (service "lambda.amazonaws.com"))
         (action "sts:AssumeRole"))))
    (managed-policies
      ("arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"))
    (inline-policies
      ((name "s3-access")
       (policy
         (version "2012-10-17")
         (statement
           ((effect "Allow")
            (action '("s3:GetObject" "s3:PutObject" "s3:DeleteObject"))
            (resource (list (format "~a/*" (ref s3-bucket "uploads" "arn"))
                          (format "~a/*" (ref s3-bucket "website" "arn"))))))))
      ((name "dynamodb-access")
       (policy
         (version "2012-10-17")
         (statement
           ((effect "Allow")
            (action '("dynamodb:Query" "dynamodb:PutItem" 
                     "dynamodb:GetItem" "dynamodb:UpdateItem"
                     "dynamodb:DeleteItem"))
            (resource (ref dynamodb-table "users" "arn"))))))))

  ;; API Gateway for REST API
  (resource api-gateway "api"
    (name (format "~a-~a-api" (var app-name) (var environment)))
    (description "Web application REST API")
    (endpoints
      ;; User management
      ((path "/users")
       (method "GET")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "api" "invoke-arn"))
         (credentials (ref iam-role "lambda-execution-role" "arn"))))
      ((path "/users")
       (method "POST")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "api" "invoke-arn"))))
      ((path "/users/{id}")
       (method "GET")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "api" "invoke-arn"))))
      ((path "/users/{id}")
       (method "PUT")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "api" "invoke-arn"))))
      
      ;; File upload
      ((path "/upload")
       (method "POST")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "api" "invoke-arn"))))
      
      ;; Health check
      ((path "/health")
       (method "GET")
       (integration
         (type "MOCK")
         (response ((status 200) 
                   (body "{\"status\": \"healthy\", \"timestamp\": \"${context.requestTime}\"}")))))
      
      ;; CORS preflight
      ((path "/{proxy+}")
       (method "OPTIONS")
       (integration
         (type "MOCK")
         (response ((status 200)
                   (headers ((Access-Control-Allow-Origin "*")
                           (Access-Control-Allow-Methods "GET,POST,PUT,DELETE,OPTIONS")
                           (Access-Control-Allow-Headers "Content-Type,X-Amz-Date,Authorization"))))))))
    
    (stage
      (name (var environment))
      (throttling
        (rate-limit 1000)
        (burst-limit 2000))
      (caching
        (enabled #t)
        (ttl 300)))
    
    (tags
      (Environment (var environment))
      (Application (var app-name))
      (Component "api-gateway")))

  ;; CloudWatch Log Groups
  (resource cloudwatch-log-group "api-logs"
    (log-group-name (format "/aws/lambda/~a-~a-api" (var app-name) (var environment)))
    (retention-days (var retention-days))
    (tags
      (Environment (var environment))
      (Application (var app-name))))

  (resource cloudwatch-log-group "processor-logs"
    (log-group-name (format "/aws/lambda/~a-~a-processor" (var app-name) (var environment)))
    (retention-days (var retention-days))
    (tags
      (Environment (var environment))
      (Application (var app-name))))

  ;; CloudWatch Alarms
  (resource cloudwatch-alarm "api-errors"
    (alarm-name (format "~a-~a-api-errors" (var app-name) (var environment)))
    (alarm-description "High error rate in API Lambda")
    (metric-name "Errors")
    (namespace "AWS/Lambda")
    (statistic "Sum")
    (period 300)
    (evaluation-periods 2)
    (threshold 10)
    (comparison-operator "GreaterThanThreshold")
    (dimensions
      (FunctionName (ref lambda-function "api" "name")))
    (alarm-actions
      ((sns-topic "alerts"))))

  ;; Outputs for integration
  (outputs
    (api-endpoint
      (value (ref api-gateway "api" "endpoint"))
      (description "API Gateway endpoint URL"))
    (website-url
      (value (ref s3-bucket "website" "website-endpoint"))
      (description "Static website URL"))
    (uploads-bucket
      (value (ref s3-bucket "uploads" "name"))
      (description "S3 bucket for file uploads"))
    (users-table
      (value (ref dynamodb-table "users" "name"))
      (description "DynamoDB users table"))
    (lambda-role-arn
      (value (ref iam-role "lambda-execution-role" "arn"))
      (description "Lambda execution role ARN")))

  ;; Lifecycle hooks
  (hooks
    (pre-create
      (shell "echo 'Deploying ~a to ~a environment'" (var app-name) (var environment)))
    (post-create
      (shell "echo 'Deployment complete!'")
      (shell "echo 'API Endpoint: ~a'" (ref api-gateway "api" "endpoint"))
      (shell "echo 'Website URL: ~a'" (ref s3-bucket "website" "website-endpoint")))
    (pre-destroy
      (confirm "Are you sure you want to destroy the ~a ~a environment?" 
               (var app-name) (var environment)))
    (post-destroy
      (shell "echo 'Environment ~a destroyed successfully'" (var environment)))))

;; Environment-specific configurations
(when (workspace? "production")
  (set-variable! 'api-memory 1024)
  (set-variable! 'retention-days 90)
  
  ;; Enable backup for production
  (modify-resource! 'dynamodb-table "users"
    (point-in-time-recovery #t)
    (backup-policy
      (enabled #t)
      (retention-days 30)))
  
  ;; Enable monitoring
  (modify-resource! 'api-gateway "api"
    (stage
      (access-logging #t)
      (metrics-enabled #t)
      (data-trace-enabled #t))))

(when (workspace? "staging")
  (set-variable! 'environment "staging")
  (set-variable! 'api-memory 512)
  (set-variable! 'retention-days 14))