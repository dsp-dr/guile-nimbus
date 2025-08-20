# Nimbus Tutorial: Your First Infrastructure

Learn Nimbus by building a simple web application infrastructure step by step.

## What We'll Build

By the end of this tutorial, you'll have:
- 📦 S3 bucket for file storage
- ⚡ Lambda function for processing
- 🌐 API Gateway for HTTP access
- 🗄️ DynamoDB table for data
- 📊 CloudWatch monitoring

## Prerequisites

Before starting, make sure you have:
- Guile 3.0+ installed
- LocalStack running
- Nimbus compiled and ready

```bash
# Verify prerequisites
guile --version        # Should show 3.0+
localstack status      # Should show services running
nimbus --version       # Should show Nimbus version
```

## Step 1: Project Setup

Create a new directory for your project:

```bash
mkdir nimbus-tutorial
cd nimbus-tutorial
```

Initialize a Nimbus project:

```bash
nimbus init
```

This creates:
- `infrastructure.nim` - Where we'll define our resources
- `nimbus.config` - Project configuration
- `.nimbus/` - State and metadata directory

Let's look at what was created:

```bash
ls -la
cat infrastructure.nim
```

## Step 2: Understanding the Infrastructure Definition

Open `infrastructure.nim` in your editor. You'll see:

```scheme
;;; infrastructure.nim - Nimbus Infrastructure Definition

(define-infrastructure default
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; Define your resources here
  ;; Example:
  ;; (resource s3-bucket "my-bucket"
  ;;   (versioning #t))
)
```

This is a Scheme-based DSL (Domain Specific Language) for defining infrastructure. Let's break it down:

- `define-infrastructure` - Defines an infrastructure stack
- `provider` - Configures the cloud provider (LocalStack in this case)
- `resource` - Defines individual infrastructure resources

## Step 3: Adding Your First Resource

Let's add an S3 bucket. Replace the example comment with:

```scheme
;;; infrastructure.nim - Nimbus Infrastructure Definition

(define-infrastructure tutorial-app
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; S3 bucket for storing application data
  (resource s3-bucket "app-data"
    (bucket-name "tutorial-app-data")
    (versioning #t)
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial"))))
```

Let's preview what this will create:

```bash
nimbus plan
```

You should see output like:

```
Planning infrastructure changes...

Nimbus will perform the following actions:

  + Create s3_bucket.app-data
      bucket_name: "tutorial-app-data"
      versioning:  true

Plan: 1 to add, 0 to change, 0 to destroy.
```

Now apply the changes:

```bash
nimbus apply --auto-approve
```

Verify the bucket was created:

```bash
# Check Nimbus state
nimbus state list

# Check in LocalStack
awslocal s3 ls
```

## Step 4: Adding a Lambda Function

Let's add a Lambda function that will process data. First, create the function code:

```bash
mkdir lambda-code
cat > lambda-code/index.py << 'EOF'
import json
import boto3

def handler(event, context):
    print(f"Received event: {json.dumps(event)}")
    
    # Simple processing logic
    if 'name' in event:
        message = f"Hello, {event['name']}!"
    else:
        message = "Hello, World!"
    
    return {
        'statusCode': 200,
        'headers': {
            'Content-Type': 'application/json'
        },
        'body': json.dumps({
            'message': message,
            'timestamp': str(context.aws_request_id)
        })
    }
EOF
```

Now add the Lambda function to your infrastructure definition:

```scheme
;;; infrastructure.nim - Nimbus Infrastructure Definition

(define-infrastructure tutorial-app
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; S3 bucket for storing application data
  (resource s3-bucket "app-data"
    (bucket-name "tutorial-app-data")
    (versioning #t)
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial")))

  ;; Lambda function for processing
  (resource lambda-function "processor"
    (function-name "tutorial-processor")
    (runtime "python3.9")
    (handler "index.handler")
    (code-uri "./lambda-code/")
    (memory-size 256)
    (timeout 30)
    (environment-variables
      (BUCKET_NAME (ref s3-bucket "app-data" "name")))
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial"))))
```

Notice the `(ref s3-bucket "app-data" "name")` - this creates a reference to our S3 bucket's name, establishing a dependency.

Apply the changes:

```bash
nimbus plan
nimbus apply --auto-approve
```

## Step 5: Adding an API Gateway

Let's add an API Gateway to make our Lambda function accessible via HTTP:

```scheme
;;; infrastructure.nim - Nimbus Infrastructure Definition

(define-infrastructure tutorial-app
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; S3 bucket for storing application data
  (resource s3-bucket "app-data"
    (bucket-name "tutorial-app-data")
    (versioning #t)
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial")))

  ;; Lambda function for processing
  (resource lambda-function "processor"
    (function-name "tutorial-processor")
    (runtime "python3.9")
    (handler "index.handler"
    (code-uri "./lambda-code/")
    (memory-size 256)
    (timeout 30)
    (environment-variables
      (BUCKET_NAME (ref s3-bucket "app-data" "name")))
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial")))

  ;; API Gateway for HTTP access
  (resource api-gateway "api"
    (name "tutorial-api")
    (description "Tutorial API Gateway")
    (endpoints
      ((path "/hello")
       (method "GET")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "processor" "invoke-arn"))))
      ((path "/hello")
       (method "POST")
       (integration
         (type "AWS_PROXY")
         (uri (ref lambda-function "processor" "invoke-arn"))))
      ((path "/health")
       (method "GET")
       (integration
         (type "MOCK")
         (response ((status 200) (body "{\"status\": \"healthy\"}"))))))
    (stage
      (name "dev")
      (throttling
        (rate-limit 100)
        (burst-limit 200)))
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial"))))
```

Apply the changes:

```bash
nimbus plan
nimbus apply --auto-approve
```

## Step 6: Adding a Database

Let's add a DynamoDB table for persistent storage:

```scheme
  ;; DynamoDB table for data storage
  (resource dynamodb-table "app-data"
    (table-name "tutorial-app-data")
    (hash-key "id" "S")
    (billing-mode "PAY_PER_REQUEST")
    (tags
      (Environment "tutorial")
      (Project "nimbus-tutorial")))
```

Add this after the Lambda function definition and apply:

```bash
nimbus plan
nimbus apply --auto-approve
```

## Step 7: Testing Your Infrastructure

Now let's test our infrastructure:

```bash
# Get the API endpoint
API_ENDPOINT=$(nimbus state show api-gateway.api | grep endpoint | cut -d'"' -f2)

# Test the health endpoint
curl $API_ENDPOINT/health

# Test the hello endpoint
curl $API_ENDPOINT/hello

# Test with a name parameter
curl -X POST $API_ENDPOINT/hello \
  -H "Content-Type: application/json" \
  -d '{"name": "Nimbus"}'
```

## Step 8: Adding Outputs

Let's make it easier to access our resources by adding outputs:

```scheme
  ;; Outputs for easy access
  (outputs
    (api-endpoint
      (value (ref api-gateway "api" "endpoint"))
      (description "API Gateway endpoint URL"))
    (bucket-name
      (value (ref s3-bucket "app-data" "name"))
      (description "S3 bucket name"))
    (function-arn
      (value (ref lambda-function "processor" "arn"))
      (description "Lambda function ARN"))
    (table-name
      (value (ref dynamodb-table "app-data" "name"))
      (description "DynamoDB table name")))
```

Apply the changes:

```bash
nimbus apply --auto-approve
```

## Step 9: Using Variables

Let's make our infrastructure more flexible by using variables:

```scheme
;;; infrastructure.nim - Nimbus Infrastructure Definition

(define-infrastructure tutorial-app
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; Variables for customization
  (variables
    (app-name
      (type string)
      (default "tutorial")
      (description "Application name prefix"))
    (environment
      (type string)
      (default "dev")
      (description "Environment name"))
    (lambda-memory
      (type number)
      (default 256)
      (description "Lambda memory allocation")))

  ;; S3 bucket for storing application data
  (resource s3-bucket "app-data"
    (bucket-name (format "~a-~a-data" (var app-name) (var environment)))
    (versioning #t)
    (tags
      (Environment (var environment))
      (Project (var app-name))))

  ;; Lambda function for processing
  (resource lambda-function "processor"
    (function-name (format "~a-~a-processor" (var app-name) (var environment)))
    (runtime "python3.9")
    (handler "index.handler")
    (code-uri "./lambda-code/")
    (memory-size (var lambda-memory))
    (timeout 30)
    (environment-variables
      (BUCKET_NAME (ref s3-bucket "app-data" "name"))
      (TABLE_NAME (ref dynamodb-table "app-data" "name")))
    (tags
      (Environment (var environment))
      (Project (var app-name))))

  ;; API Gateway for HTTP access
  (resource api-gateway "api"
    (name (format "~a-~a-api" (var app-name) (var environment)))
    ;; ... rest of configuration
    )

  ;; DynamoDB table for data storage
  (resource dynamodb-table "app-data"
    (table-name (format "~a-~a-data" (var app-name) (var environment)))
    ;; ... rest of configuration
    )

  ;; Outputs
  (outputs
    ;; ... outputs as before
    ))
```

Apply the changes:

```bash
nimbus plan
nimbus apply --auto-approve
```

## Step 10: Managing State

Let's explore Nimbus state management:

```bash
# List all resources
nimbus state list

# Show details of a specific resource
nimbus state show s3-bucket.app-data
nimbus state show lambda-function.processor

# Validate our configuration
nimbus validate
```

## Step 11: Working with Workspaces

Workspaces allow you to manage multiple environments:

```bash
# Create a staging workspace
nimbus workspace new staging
nimbus workspace select staging

# Deploy to staging
nimbus apply --auto-approve

# List workspaces
nimbus workspace list

# Switch back to default
nimbus workspace select default
```

## Step 12: Cleanup

When you're done experimenting, clean up your resources:

```bash
# Destroy staging environment
nimbus workspace select staging
nimbus destroy --auto-approve

# Destroy default environment
nimbus workspace select default
nimbus destroy --auto-approve
```

## What's Next?

Congratulations! You've built your first Nimbus infrastructure. Here's what to explore next:

### Advanced Features

1. **Modules** - Reusable infrastructure components
2. **Policies** - Governance and compliance rules
3. **Remote State** - Team collaboration
4. **Providers** - AWS, Azure, GCP support

### Real-World Examples

- [Web Application](../examples/web-app/) - Complete web app with frontend and backend
- [Microservices](../examples/microservices/) - Service mesh architecture
- [Data Pipeline](../examples/data-pipeline/) - ETL and analytics

### Development

- [Contributing Guide](../CONTRIBUTING.md) - How to contribute to Nimbus
- [Development Guide](DEVELOPMENT.md) - Technical deep-dive
- [Architecture Guide](ARCHITECTURE.md) - System design

## Getting Help

- **Documentation**: [docs/](https://github.com/dsp-dr/guile-nimbus/tree/main/docs)
- **Issues**: [GitHub Issues](https://github.com/dsp-dr/guile-nimbus/issues)
- **Discussions**: [GitHub Discussions](https://github.com/dsp-dr/guile-nimbus/discussions)

Happy infrastructure coding with Nimbus! 🚀