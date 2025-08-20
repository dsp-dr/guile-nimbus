;;; infrastructure.nim - Example Nimbus Infrastructure Definition
;;; This demonstrates what a production-ready DSL would look like

(define-infrastructure my-app
  ;; Provider configuration
  (provider localstack
    (endpoint (env-var "LOCALSTACK_ENDPOINT" "http://localhost:4566"))
    (region "us-east-1")
    (access-key "test")
    (secret-key "test"))

  ;; Variables for reusability
  (variables
    (environment 
      (type string)
      (default "development")
      (description "Deployment environment"))
    (app-name
      (type string)
      (default "nimbus-demo")
      (description "Application name"))
    (retention-days
      (type number)
      (default 7)
      (description "Log retention in days")))

  ;; S3 Bucket for application data
  (resource s3-bucket app-data
    (bucket-name (format "~a-~a-data" (var app-name) (var environment)))
    (versioning #t)
    (lifecycle-rules
      ((id "cleanup-old-versions")
       (status "Enabled")
       (expiration-days 30)))
    (tags
      (Environment (var environment))
      (ManagedBy "Nimbus")
      (Application (var app-name))))

  ;; DynamoDB table for state
  (resource dynamodb-table app-state
    (table-name (format "~a-~a-state" (var app-name) (var environment)))
    (hash-key "id" "S")
    (range-key "timestamp" "N")
    (billing-mode "PAY_PER_REQUEST")
    (global-secondary-indexes
      ((name "by-status")
       (hash-key "status" "S")
       (projection-type "ALL")))
    (tags
      (Environment (var environment))
      (ManagedBy "Nimbus")))

  ;; Lambda function for processing
  (resource lambda-function processor
    (function-name (format "~a-~a-processor" (var app-name) (var environment)))
    (runtime "python3.9")
    (handler "main.handler")
    (code 
      (from-directory "./lambda-code/"))
    (memory-size 256)
    (timeout 30)
    (environment-variables
      (BUCKET_NAME (ref s3-bucket app-data "name"))
      (TABLE_NAME (ref dynamodb-table app-state "name"))
      (ENVIRONMENT (var environment)))
    (layers
      ("arn:aws:lambda:us-east-1:123456789:layer:common-utils:1"))
    (tags
      (Environment (var environment))
      (ManagedBy "Nimbus")))

  ;; IAM Role for Lambda
  (resource iam-role lambda-execution-role
    (role-name (format "~a-~a-lambda-role" (var app-name) (var environment)))
    (assume-role-policy
      (version "2012-10-17")
      (statement
        ((effect "Allow")
         (principal (service "lambda.amazonaws.com"))
         (action "sts:AssumeRole"))))
    (policies
      ((name "s3-access")
       (policy
         (version "2012-10-17")
         (statement
           ((effect "Allow")
            (action ("s3:GetObject" "s3:PutObject"))
            (resource (format "~a/*" (ref s3-bucket app-data "arn")))))))
      ((name "dynamodb-access")
       (policy
         (version "2012-10-17")
         (statement
           ((effect "Allow")
            (action ("dynamodb:*"))
            (resource (ref dynamodb-table app-state "arn"))))))))

  ;; API Gateway
  (resource api-gateway rest-api
    (name (format "~a-~a-api" (var app-name) (var environment)))
    (description "REST API for application")
    (endpoints
      ((path "/process")
       (method "POST")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function processor "invoke-arn"))))
      ((path "/status")
       (method "GET")
       (integration
         (type "MOCK")
         (response ((status 200) (body "{\"status\": \"healthy\"}"))))))
    (stage
      (name (var environment))
      (throttling
        (rate-limit 1000)
        (burst-limit 2000)))
    (tags
      (Environment (var environment))
      (ManagedBy "Nimbus")))

  ;; CloudWatch Log Group
  (resource cloudwatch-log-group app-logs
    (log-group-name (format "/aws/lambda/~a-~a" (var app-name) (var environment)))
    (retention-days (var retention-days))
    (tags
      (Environment (var environment))
      (ManagedBy "Nimbus")))

  ;; Outputs for reference
  (outputs
    (api-endpoint
      (value (ref api-gateway rest-api "endpoint"))
      (description "API Gateway endpoint URL"))
    (bucket-name
      (value (ref s3-bucket app-data "name"))
      (description "S3 bucket name"))
    (table-name
      (value (ref dynamodb-table app-state "name"))
      (description "DynamoDB table name"))
    (function-arn
      (value (ref lambda-function processor "arn"))
      (description "Lambda function ARN")))

  ;; Lifecycle hooks
  (hooks
    (pre-create
      (shell "echo 'Deploying to ~a environment'" (var environment)))
    (post-create
      (shell "curl -X POST ~a/status" (ref api-gateway rest-api "endpoint")))
    (pre-destroy
      (confirm "Are you sure you want to destroy ~a environment?" (var environment)))))

;;; Module imports for reusable components
(use-modules 
  (nimbus modules networking)  ; VPC, subnets, security groups
  (nimbus modules monitoring)  ; CloudWatch alarms, dashboards
  (nimbus modules security))   ; KMS keys, secrets manager

;;; Policy definitions
(define-policy cost-control
  (description "Prevent expensive resource creation")
  (rules
    ((action "create")
     (resource-type "ec2-instance")
     (condition (instance-type-not-in '("t3.micro" "t3.small")))
     (effect "deny")
     (message "Only t3.micro and t3.small instances allowed"))
    ((action "create")
     (resource-type "rds-instance")
     (effect "require-approval")
     (approvers '("admin@example.com")))))

;;; Workspace-specific overrides
(when (workspace? "production")
  (set-variable! 'retention-days 90)
  (set-variable! 'environment "production")
  (enable-backup! 's3-bucket 'app-data)
  (enable-encryption! 'dynamodb-table 'app-state))