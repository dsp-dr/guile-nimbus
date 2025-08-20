;;; microservices/infrastructure.nim - Microservices Architecture Example
;;; Demonstrates service mesh, load balancing, and inter-service communication

(define-infrastructure microservices
  ;; Provider configuration
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; Shared variables
  (variables
    (environment (type string) (default "development"))
    (cluster-name (type string) (default "microservices"))
    (vpc-cidr (type string) (default "10.0.0.0/16")))

  ;; Shared components module
  (module shared-components
    ;; VPC and networking
    (resource vpc "main"
      (cidr-block (var vpc-cidr))
      (enable-dns-hostnames #t)
      (enable-dns-support #t)
      (tags
        (Name (format "~a-vpc" (var cluster-name)))
        (Environment (var environment))))

    (resource subnet "private-a"
      (vpc-id (ref vpc "main" "id"))
      (cidr-block "10.0.1.0/24")
      (availability-zone "us-east-1a")
      (tags (Name "private-subnet-a")))

    (resource subnet "private-b"
      (vpc-id (ref vpc "main" "id"))
      (cidr-block "10.0.2.0/24")
      (availability-zone "us-east-1b")
      (tags (Name "private-subnet-b")))

    ;; Service discovery
    (resource service-discovery-namespace "services"
      (name (format "~a.local" (var cluster-name)))
      (type "DNS_PRIVATE")
      (vpc-id (ref vpc "main" "id"))
      (description "Service discovery for microservices"))

    ;; Application Load Balancer
    (resource application-load-balancer "main"
      (name (format "~a-alb" (var cluster-name)))
      (scheme "internal")
      (type "application")
      (subnets (list (ref subnet "private-a" "id")
                    (ref subnet "private-b" "id")))
      (security-groups (list (ref security-group "alb" "id")))
      (tags
        (Environment (var environment))))

    ;; Security groups
    (resource security-group "alb"
      (name (format "~a-alb-sg" (var cluster-name)))
      (description "ALB security group")
      (vpc-id (ref vpc "main" "id"))
      (ingress-rules
        ((from-port 80) (to-port 80) (protocol "tcp") (cidr-blocks '("10.0.0.0/16")))
        ((from-port 443) (to-port 443) (protocol "tcp") (cidr-blocks '("10.0.0.0/16"))))
      (egress-rules
        ((from-port 0) (to-port 65535) (protocol "tcp") (cidr-blocks '("0.0.0.0/0")))))

    (resource security-group "services"
      (name (format "~a-services-sg" (var cluster-name)))
      (description "Microservices security group")
      (vpc-id (ref vpc "main" "id"))
      (ingress-rules
        ((from-port 8080) (to-port 8080) (protocol "tcp") 
         (source-security-group-id (ref security-group "alb" "id")))
        ((from-port 3000) (to-port 3000) (protocol "tcp")
         (source-security-group-id (ref security-group "alb" "id"))))
      (egress-rules
        ((from-port 0) (to-port 65535) (protocol "tcp") (cidr-blocks '("0.0.0.0/0"))))))

  ;; User Service
  (module user-service
    (resource dynamodb-table "users"
      (table-name (format "~a-users" (var cluster-name)))
      (hash-key "user_id" "S")
      (billing-mode "PAY_PER_REQUEST")
      (global-secondary-indexes
        ((name "email-index")
         (hash-key "email" "S")
         (projection-type "ALL")))
      (tags
        (Service "user-service")
        (Environment (var environment))))

    (resource lambda-function "user-api"
      (function-name (format "~a-user-service" (var cluster-name)))
      (runtime "python3.9")
      (handler "user_service.handler")
      (code (from-directory "./services/user/"))
      (memory-size 512)
      (timeout 30)
      (environment-variables
        (USERS_TABLE (ref dynamodb-table "users" "name"))
        (SERVICE_NAME "user-service")
        (CLUSTER_NAME (var cluster-name)))
      (vpc-configuration
        (subnet-ids (list (ref subnet "private-a" "id")
                         (ref subnet "private-b" "id")))
        (security-group-ids (list (ref security-group "services" "id"))))
      (tags
        (Service "user-service")))

    (resource service-discovery-service "user-service"
      (name "user-service")
      (namespace-id (ref service-discovery-namespace "services" "id"))
      (dns-config
        (namespace-id (ref service-discovery-namespace "services" "id"))
        (dns-records
          ((type "A") (ttl 60))
          ((type "SRV") (ttl 60))))
      (health-check-config
        (type "HTTP")
        (resource-path "/health")
        (failure-threshold 3)))

    (resource alb-target-group "user-service"
      (name (format "~a-user-tg" (var cluster-name)))
      (port 8080)
      (protocol "HTTP")
      (vpc-id (ref vpc "main" "id"))
      (target-type "lambda")
      (health-check
        (enabled #t)
        (healthy-threshold 2)
        (interval 30)
        (matcher "200")
        (path "/health")
        (port "traffic-port")
        (protocol "HTTP")
        (timeout 5)
        (unhealthy-threshold 2))
      (targets
        ((id (ref lambda-function "user-api" "arn"))))))

  ;; Order Service  
  (module order-service
    (resource dynamodb-table "orders"
      (table-name (format "~a-orders" (var cluster-name)))
      (hash-key "order_id" "S")
      (range-key "created_at" "S")
      (billing-mode "PAY_PER_REQUEST")
      (global-secondary-indexes
        ((name "user-orders")
         (hash-key "user_id" "S")
         (range-key "created_at" "S")
         (projection-type "ALL"))
        ((name "status-index")
         (hash-key "status" "S")
         (projection-type "KEYS_ONLY")))
      (stream-specification
        (stream-enabled #t)
        (stream-view-type "NEW_AND_OLD_IMAGES"))
      (tags
        (Service "order-service")
        (Environment (var environment))))

    (resource lambda-function "order-api"
      (function-name (format "~a-order-service" (var cluster-name)))
      (runtime "python3.9")
      (handler "order_service.handler")
      (code (from-directory "./services/order/"))
      (memory-size 512)
      (timeout 30)
      (environment-variables
        (ORDERS_TABLE (ref dynamodb-table "orders" "name"))
        (USER_SERVICE_URL (format "http://user-service.~a.local:8080" (var cluster-name)))
        (PAYMENT_SERVICE_URL (format "http://payment-service.~a.local:8080" (var cluster-name)))
        (SERVICE_NAME "order-service"))
      (vpc-configuration
        (subnet-ids (list (ref subnet "private-a" "id")
                         (ref subnet "private-b" "id")))
        (security-group-ids (list (ref security-group "services" "id"))))
      (tags
        (Service "order-service")))

    ;; Order event processor
    (resource lambda-function "order-processor"
      (function-name (format "~a-order-processor" (var cluster-name)))
      (runtime "python3.9")
      (handler "order_processor.handler")
      (code (from-directory "./services/order-processor/"))
      (memory-size 256)
      (timeout 60)
      (environment-variables
        (ORDERS_TABLE (ref dynamodb-table "orders" "name"))
        (NOTIFICATION_TOPIC (ref sns-topic "order-events" "arn")))
      (event-source-mappings
        ((event-source-arn (ref dynamodb-table "orders" "stream-arn"))
         (starting-position "LATEST")
         (batch-size 10)))
      (tags
        (Service "order-service")))

    (resource alb-target-group "order-service"
      (name (format "~a-order-tg" (var cluster-name)))
      (port 8080)
      (protocol "HTTP")
      (vpc-id (ref vpc "main" "id"))
      (target-type "lambda")
      (health-check
        (path "/health"))
      (targets
        ((id (ref lambda-function "order-api" "arn"))))))

  ;; Payment Service
  (module payment-service
    (resource dynamodb-table "payments"
      (table-name (format "~a-payments" (var cluster-name)))
      (hash-key "payment_id" "S")
      (billing-mode "PAY_PER_REQUEST")
      (global-secondary-indexes
        ((name "order-payments")
         (hash-key "order_id" "S")
         (projection-type "ALL")))
      (tags
        (Service "payment-service")))

    (resource lambda-function "payment-api"
      (function-name (format "~a-payment-service" (var cluster-name)))
      (runtime "python3.9")
      (handler "payment_service.handler")
      (code (from-directory "./services/payment/"))
      (memory-size 256)
      (timeout 30)
      (environment-variables
        (PAYMENTS_TABLE (ref dynamodb-table "payments" "name"))
        (SERVICE_NAME "payment-service"))
      (vpc-configuration
        (subnet-ids (list (ref subnet "private-a" "id")
                         (ref subnet "private-b" "id")))
        (security-group-ids (list (ref security-group "services" "id"))))
      (tags
        (Service "payment-service")))

    (resource alb-target-group "payment-service"
      (name (format "~a-payment-tg" (var cluster-name)))
      (port 8080)
      (protocol "HTTP")
      (vpc-id (ref vpc "main" "id"))
      (target-type "lambda")
      (targets
        ((id (ref lambda-function "payment-api" "arn"))))))

  ;; API Gateway for external access
  (resource api-gateway "main"
    (name (format "~a-api" (var cluster-name)))
    (description "Microservices API Gateway")
    (endpoints
      ;; User service routes
      ((path "/users")
       (method "GET")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/users" 
                     (ref application-load-balancer "main" "dns-name")))))
      ((path "/users/{id}")
       (method "GET")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/users/{id}"
                     (ref application-load-balancer "main" "dns-name")))))
      ((path "/users")
       (method "POST")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/users"
                     (ref application-load-balancer "main" "dns-name")))))
      
      ;; Order service routes
      ((path "/orders")
       (method "GET")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/orders"
                     (ref application-load-balancer "main" "dns-name")))))
      ((path "/orders")
       (method "POST")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/orders"
                     (ref application-load-balancer "main" "dns-name")))))
      ((path "/orders/{id}")
       (method "GET")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/orders/{id}"
                     (ref application-load-balancer "main" "dns-name")))))
      
      ;; Payment service routes
      ((path "/payments")
       (method "POST")
       (integration
         (type "HTTP_PROXY")
         (uri (format "http://~a/payments"
                     (ref application-load-balancer "main" "dns-name"))))))
    
    (stage
      (name (var environment))
      (throttling
        (rate-limit 5000)
        (burst-limit 10000)))
    
    (tags
      (Environment (var environment))))

  ;; Messaging and Events
  (resource sns-topic "order-events"
    (name (format "~a-order-events" (var cluster-name)))
    (display-name "Order Events")
    (tags
      (Environment (var environment))))

  (resource sqs-queue "order-notifications"
    (queue-name (format "~a-order-notifications" (var cluster-name)))
    (visibility-timeout-seconds 300)
    (message-retention-period 1209600)  ; 14 days
    (tags
      (Environment (var environment))))

  (resource sns-subscription "order-notifications"
    (topic-arn (ref sns-topic "order-events" "arn"))
    (protocol "sqs")
    (endpoint (ref sqs-queue "order-notifications" "arn")))

  ;; Monitoring and Observability
  (resource cloudwatch-dashboard "microservices"
    (dashboard-name (format "~a-dashboard" (var cluster-name)))
    (dashboard-body
      (widgets
        ((type "metric")
         (properties
           (metrics
             (("AWS/Lambda" "Duration" "FunctionName" (ref lambda-function "user-api" "name"))
              ("AWS/Lambda" "Duration" "FunctionName" (ref lambda-function "order-api" "name"))
              ("AWS/Lambda" "Duration" "FunctionName" (ref lambda-function "payment-api" "name"))))
           (period 300)
           (stat "Average")
           (region "us-east-1")
           (title "Lambda Duration")))
        ((type "metric")
         (properties
           (metrics
             (("AWS/Lambda" "Errors" "FunctionName" (ref lambda-function "user-api" "name"))
              ("AWS/Lambda" "Errors" "FunctionName" (ref lambda-function "order-api" "name"))
              ("AWS/Lambda" "Errors" "FunctionName" (ref lambda-function "payment-api" "name"))))
           (period 300)
           (stat "Sum")
           (title "Lambda Errors"))))))

  ;; Outputs
  (outputs
    (api-gateway-endpoint
      (value (ref api-gateway "main" "endpoint"))
      (description "Public API endpoint"))
    (load-balancer-dns
      (value (ref application-load-balancer "main" "dns-name"))
      (description "Internal load balancer DNS"))
    (vpc-id
      (value (ref vpc "main" "id"))
      (description "VPC ID"))
    (service-discovery-namespace
      (value (ref service-discovery-namespace "services" "name"))
      (description "Service discovery namespace"))
    (dashboard-url
      (value (ref cloudwatch-dashboard "microservices" "url"))
      (description "CloudWatch dashboard URL")))

  ;; Service dependencies
  (dependencies
    (order-service (depends-on user-service payment-service))
    (payment-service (depends-on user-service))))

;; Environment-specific configurations
(when (workspace? "production")
  ;; Increase resources for production
  (modify-resource! 'lambda-function "user-api"
    (memory-size 1024)
    (reserved-concurrency 100))
  
  (modify-resource! 'lambda-function "order-api"
    (memory-size 1024)
    (reserved-concurrency 200))
  
  ;; Enable auto-scaling for DynamoDB
  (modify-resource! 'dynamodb-table "users"
    (billing-mode "PROVISIONED")
    (read-capacity 20)
    (write-capacity 20)
    (auto-scaling
      (read (min-capacity 5) (max-capacity 100) (target-utilization 70))
      (write (min-capacity 5) (max-capacity 100) (target-utilization 70))))
  
  ;; Enable X-Ray tracing
  (modify-resource! 'lambda-function "user-api"
    (tracing-config (mode "Active")))
  
  (modify-resource! 'lambda-function "order-api" 
    (tracing-config (mode "Active"))))

(when (workspace? "development")
  ;; Reduce costs for development
  (modify-resource! 'lambda-function "user-api"
    (memory-size 256))
  
  (modify-resource! 'lambda-function "order-api"
    (memory-size 256))
  
  ;; Shorter retention for dev
  (modify-resource! 'sqs-queue "order-notifications"
    (message-retention-period 604800)))  ; 7 days