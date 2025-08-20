# Web Application Example

This example demonstrates a complete web application infrastructure using Nimbus, including:

- **Frontend**: Static website hosted on S3
- **Backend**: REST API using Lambda and API Gateway  
- **Database**: DynamoDB for user data
- **Storage**: S3 for file uploads
- **Processing**: Lambda for background file processing
- **Monitoring**: CloudWatch logs and alarms

## Architecture

```
┌─────────────┐    ┌──────────────┐    ┌─────────────┐
│   Frontend  │    │ API Gateway  │    │   Lambda    │
│  (S3 Web)   │───▶│   (REST)     │───▶│    (API)    │
└─────────────┘    └──────────────┘    └─────────────┘
                                              │
                   ┌──────────────┐          │
                   │  DynamoDB    │◀─────────┘
                   │   (Users)    │
                   └──────────────┘
                           
┌─────────────┐    ┌──────────────┐    ┌─────────────┐
│   Upload    │    │   S3 Event   │    │   Lambda    │
│  (S3 Bucket)│───▶│   Trigger    │───▶│ (Processor) │
└─────────────┘    └──────────────┘    └─────────────┘
```

## Prerequisites

1. **LocalStack** - Running on port 4566
2. **Nimbus** - Built and ready to use
3. **Application Code** - API and processor functions

## Setup Instructions

### 1. Start LocalStack

```bash
localstack start
```

### 2. Prepare Application Code

Create the API Lambda function:

```bash
mkdir -p api
cat > api/app.py << 'EOF'
import json
import boto3
import os
from datetime import datetime

dynamodb = boto3.resource('dynamodb', endpoint_url='http://localhost:4566')
s3 = boto3.client('s3', endpoint_url='http://localhost:4566')

def handler(event, context):
    print(f"Event: {json.dumps(event)}")
    
    method = event.get('httpMethod')
    path = event.get('path')
    
    try:
        if path == '/health':
            return {
                'statusCode': 200,
                'headers': {'Content-Type': 'application/json'},
                'body': json.dumps({
                    'status': 'healthy',
                    'timestamp': datetime.now().isoformat()
                })
            }
        
        elif path == '/users' and method == 'GET':
            return list_users()
        
        elif path == '/users' and method == 'POST':
            return create_user(json.loads(event.get('body', '{}')))
        
        elif path.startswith('/users/') and method == 'GET':
            user_id = path.split('/')[-1]
            return get_user(user_id)
        
        elif path == '/upload' and method == 'POST':
            return handle_upload(event)
        
        else:
            return {
                'statusCode': 404,
                'body': json.dumps({'error': 'Not found'})
            }
            
    except Exception as e:
        print(f"Error: {str(e)}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }

def list_users():
    table = dynamodb.Table(os.environ['USERS_TABLE'])
    response = table.scan()
    
    return {
        'statusCode': 200,
        'headers': {'Content-Type': 'application/json'},
        'body': json.dumps(response['Items'])
    }

def create_user(user_data):
    table = dynamodb.Table(os.environ['USERS_TABLE'])
    
    user = {
        'user_id': user_data.get('user_id'),
        'email': user_data.get('email'),
        'name': user_data.get('name'),
        'created_at': datetime.now().isoformat()
    }
    
    table.put_item(Item=user)
    
    return {
        'statusCode': 201,
        'headers': {'Content-Type': 'application/json'},
        'body': json.dumps(user)
    }

def get_user(user_id):
    table = dynamodb.Table(os.environ['USERS_TABLE'])
    response = table.get_item(Key={'user_id': user_id})
    
    if 'Item' in response:
        return {
            'statusCode': 200,
            'headers': {'Content-Type': 'application/json'},
            'body': json.dumps(response['Item'])
        }
    else:
        return {
            'statusCode': 404,
            'body': json.dumps({'error': 'User not found'})
        }

def handle_upload(event):
    # Generate pre-signed URL for S3 upload
    bucket = os.environ['UPLOADS_BUCKET']
    key = f"uploads/{datetime.now().isoformat()}"
    
    url = s3.generate_presigned_url(
        'put_object',
        Params={'Bucket': bucket, 'Key': key},
        ExpiresIn=3600
    )
    
    return {
        'statusCode': 200,
        'headers': {'Content-Type': 'application/json'},
        'body': json.dumps({
            'upload_url': url,
            'key': key
        })
    }
EOF
```

Create the file processor:

```bash
mkdir -p processor
cat > processor/processor.py << 'EOF'
import json
import boto3
import os
from PIL import Image
import io

s3 = boto3.client('s3', endpoint_url='http://localhost:4566')

def handler(event, context):
    print(f"Processing event: {json.dumps(event)}")
    
    for record in event.get('Records', []):
        bucket = record['s3']['bucket']['name']
        key = record['s3']['object']['key']
        
        try:
            process_image(bucket, key)
        except Exception as e:
            print(f"Error processing {key}: {str(e)}")
            raise

def process_image(bucket, key):
    print(f"Processing image: {bucket}/{key}")
    
    # Download image
    response = s3.get_object(Bucket=bucket, Key=key)
    image_data = response['Body'].read()
    
    # Process with PIL
    image = Image.open(io.BytesIO(image_data))
    
    # Create thumbnail
    thumbnail = image.copy()
    thumbnail.thumbnail((200, 200))
    
    # Save thumbnail
    thumb_buffer = io.BytesIO()
    thumbnail.save(thumb_buffer, format='JPEG')
    thumb_buffer.seek(0)
    
    thumb_key = key.replace('raw/', 'thumbnails/thumb_')
    s3.put_object(
        Bucket=bucket,
        Key=thumb_key,
        Body=thumb_buffer.getvalue(),
        ContentType='image/jpeg'
    )
    
    print(f"Created thumbnail: {thumb_key}")
EOF
```

### 3. Deploy Infrastructure

```bash
# Initialize Nimbus project
nimbus init

# Copy the infrastructure definition
cp infrastructure.nim .

# Preview changes
nimbus plan

# Deploy
nimbus apply --auto-approve
```

### 4. Test the Application

```bash
# Get API endpoint
API_ENDPOINT=$(nimbus state show api-gateway.api | grep endpoint)

# Test health endpoint
curl $API_ENDPOINT/health

# Create a user
curl -X POST $API_ENDPOINT/users \
  -H "Content-Type: application/json" \
  -d '{"user_id": "123", "email": "test@example.com", "name": "Test User"}'

# List users
curl $API_ENDPOINT/users

# Get upload URL
curl -X POST $API_ENDPOINT/upload

# Upload a file using the pre-signed URL (would trigger processing)
```

## Environment Management

### Development Environment

```bash
# Deploy to development (default)
nimbus apply
```

### Staging Environment

```bash
# Switch to staging workspace
nimbus workspace new staging
nimbus workspace select staging

# Deploy with staging configuration
nimbus apply
```

### Production Environment  

```bash
# Create production workspace
nimbus workspace new production
nimbus workspace select production

# Deploy with production settings
# (higher memory, longer retention, backups enabled)
nimbus apply
```

## Monitoring

### View Logs

```bash
# API logs
awslocal logs tail /aws/lambda/web-app-development-api

# Processor logs  
awslocal logs tail /aws/lambda/web-app-development-processor
```

### Check Resources

```bash
# List all resources
nimbus state list

# Check specific resources
nimbus state show s3-bucket.uploads
nimbus state show dynamodb-table.users
nimbus state show lambda-function.api
```

## Cleanup

```bash
# Destroy everything
nimbus destroy --auto-approve

# Or destroy specific workspace
nimbus workspace select staging
nimbus destroy --auto-approve
```

## Customization

### Adding New Resources

Edit `infrastructure.nim` to add:

- SNS topics for notifications
- SQS queues for background jobs  
- ElastiCache for caching
- CloudFront for CDN

### Environment Variables

Modify variables for different environments:

```scheme
(when (workspace? "production")
  (set-variable! 'api-memory 2048)
  (set-variable! 'retention-days 365))
```

### Security Policies

Add IAM policies for fine-grained access control:

```scheme
(resource iam-policy "api-policy"
  (policy-name "APIAccessPolicy")
  (policy-document
    (version "2012-10-17")
    (statement
      ((effect "Allow")
       (action "dynamodb:Query")
       (resource (ref dynamodb-table "users" "arn"))
       (condition
         (StringEquals ((dynamodb:LeadingKeys "${aws:userid}"))))))))
```

This example demonstrates Nimbus's power for managing complex, multi-service applications with infrastructure as code.