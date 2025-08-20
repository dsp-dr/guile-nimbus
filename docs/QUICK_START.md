# Nimbus Quick Start Guide

Get up and running with Nimbus in 5 minutes!

## Prerequisites

### 1. Install Guile 3.0+

```bash
# FreeBSD
pkg install guile3

# Ubuntu/Debian  
apt-get install guile-3.0

# macOS
brew install guile

# Verify installation
guile --version  # Should show 3.0 or higher
```

### 2. Install LocalStack (Optional but Recommended)

```bash
# Using pip
pip install localstack

# Using Docker
docker pull localstack/localstack

# Start LocalStack
localstack start
# or
docker run -p 4566:4566 localstack/localstack
```

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/dsp-dr/guile-nimbus.git
cd guile-nimbus

# Check dependencies
make check-deps

# Compile
make compile

# Add to PATH (optional)
export PATH="$PWD/bin:$PATH"
```

## Your First Nimbus Project

### 1. Initialize a Project

```bash
# Create project directory
mkdir my-infrastructure
cd my-infrastructure

# Initialize Nimbus project
nimbus init
```

This creates:
- `infrastructure.nim` - Infrastructure definition
- `nimbus.config` - Project configuration
- `.nimbus/` - State and metadata directory

### 2. Define Infrastructure

Edit `infrastructure.nim`:

```scheme
;;; infrastructure.nim - My First Infrastructure

(define-infrastructure my-app
  ;; Configure LocalStack provider
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))

  ;; Create an S3 bucket
  (resource s3-bucket "app-data"
    (bucket-name "my-app-data")
    (versioning #t)
    (tags
      (Environment "development")
      (Project "my-app")))

  ;; Create a Lambda function
  (resource lambda-function "processor"
    (function-name "data-processor")
    (runtime "python3.9")
    (handler "index.handler")
    (code-uri "./lambda/")
    (memory-size 256)
    (timeout 30))

  ;; Output values
  (outputs
    (bucket-name (ref s3-bucket "app-data" "name"))
    (function-arn (ref lambda-function "processor" "arn"))))
```

### 3. Create Lambda Code

```bash
mkdir lambda
cat > lambda/index.py << 'EOF'
def handler(event, context):
    return {
        'statusCode': 200,
        'body': 'Hello from Nimbus!'
    }
EOF
```

### 4. Preview Changes

```bash
nimbus plan
```

Output:
```
Planning infrastructure changes...

Nimbus will perform the following actions:

  + Create s3_bucket.app-data
      bucket_name: "my-app-data"
      versioning:  true
      
  + Create lambda_function.processor
      function_name: "data-processor"
      runtime:       "python3.9"
      handler:       "index.handler"

Plan: 2 to add, 0 to change, 0 to destroy.
```

### 5. Apply Infrastructure

```bash
# Apply with confirmation
nimbus apply

# Or auto-approve
nimbus apply --auto-approve
```

Output:
```
Applying infrastructure changes...

Creating s3_bucket.app-data... [✓]
Creating lambda_function.processor... [✓]

Apply complete! Resources: 2 added, 0 changed, 0 destroyed.

Outputs:
  bucket_name = "my-app-data"
  function_arn = "arn:aws:lambda:us-east-1:000000000000:function:data-processor"
```

### 6. Manage State

```bash
# List resources
nimbus state list

# Show resource details
nimbus state show s3.app-data

# Validate configuration
nimbus validate
```

### 7. Clean Up

```bash
# Destroy all resources
nimbus destroy --auto-approve
```

## Working with Workspaces

Manage multiple environments:

```bash
# Create staging workspace
nimbus workspace new staging
nimbus workspace select staging
nimbus apply

# Switch back to default
nimbus workspace select default
nimbus workspace list
```

## Advanced Features

### Variables

Define reusable values in `infrastructure.nim`:

```scheme
(variables
  (environment 
    (type string)
    (default "development"))
  (instance-count
    (type number)
    (default 2)))

(resource s3-bucket "data"
  (bucket-name (format "app-~a-data" (var environment))))
```

### Modules

Reuse infrastructure patterns:

```scheme
;; modules/vpc.nim
(define-module vpc
  (parameters
    (cidr-block))
  
  (resource vpc "main"
    (cidr-block (param cidr-block))))

;; infrastructure.nim
(use-module "./modules/vpc.nim")

(module vpc "network"
  (cidr-block "10.0.0.0/16"))
```

### Dependencies

Express resource relationships:

```scheme
(resource lambda-function "processor"
  (environment-variables
    (BUCKET_NAME (ref s3-bucket "data" "name")))
  (depends-on s3-bucket "data"))
```

## Troubleshooting

### Common Issues

1. **Guile Version Error**
   ```
   Error: Guile 3.0 or higher is required
   ```
   Solution: Update Guile or use `guile3` command

2. **LocalStack Connection Failed**
   ```
   Warning: LocalStack is not accessible
   ```
   Solution: Start LocalStack with `localstack start`

3. **Module Not Found**
   ```
   no code for module (nimbus ...)
   ```
   Solution: Run from project root or set `GUILE_LOAD_PATH`

### Debug Mode

```bash
# Enable verbose output
nimbus plan --verbose

# Check debug logs
tail -f .nimbus/debug.log

# Interactive debugging
make repl
> (use-modules (nimbus cli commands))
> (nimbus-plan)
```

## Next Steps

- Read the [Architecture Guide](ARCHITECTURE.md)
- Explore [Example Projects](../examples/)
- Learn about [Provider Development](PROVIDER_GUIDE.md)
- Join the [Community](https://github.com/dsp-dr/guile-nimbus/discussions)

## Getting Help

- **Documentation**: [docs/](https://github.com/dsp-dr/guile-nimbus/tree/main/docs)
- **Issues**: [GitHub Issues](https://github.com/dsp-dr/guile-nimbus/issues)
- **Discussions**: [GitHub Discussions](https://github.com/dsp-dr/guile-nimbus/discussions)

Happy Infrastructure Coding! 🚀