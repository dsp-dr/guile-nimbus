# Nimbus IAC Platform - Release Roadmap

## Current Status: Alpha (v0.1.0)
✅ Core data models implemented
✅ Test suite with 172 passing tests
✅ CI/CD pipeline operational
✅ Guile 3.0+ requirement enforced

## Phase 1: Core Functionality (v0.2.0)
**Goal**: Basic working IAC with LocalStack

### 1.1 LocalStack Provider
```scheme
nimbus/
└── providers/
    └── localstack/
        ├── provider.scm      # Provider configuration
        ├── s3.scm           # S3 bucket resources
        ├── lambda.scm       # Lambda functions
        ├── dynamodb.scm     # DynamoDB tables
        └── apigateway.scm   # API Gateway
```

### 1.2 CLI Interface
```bash
nimbus init              # Initialize new project
nimbus plan              # Show execution plan
nimbus apply             # Apply changes
nimbus destroy           # Destroy infrastructure
nimbus state list        # List resources
nimbus state show <id>   # Show resource details
```

### 1.3 Storage Backend
- SQLite for local development
- File-based state with locking
- Transaction support

## Phase 2: Execution Engine (v0.3.0)
**Goal**: Reliable resource orchestration

### 2.1 DAG Executor
```scheme
(define-class <execution-graph> ()
  (nodes)     ; Resource nodes
  (edges)     ; Dependencies
  (scheduler) ; Parallel execution
)
```

### 2.2 Resource Lifecycle
- Create → Read → Update → Delete
- Pre/post hooks
- Error handling with retry
- Progress reporting

### 2.3 State Management
- Atomic operations
- Drift detection
- State migrations
- Import existing resources

## Phase 3: Configuration DSL (v0.4.0)
**Goal**: User-friendly infrastructure definitions

### 3.1 Nimbus DSL
```scheme
;; infrastructure.nim
(define-infrastructure production
  (provider localstack
    (endpoint "http://localhost:4566")
    (region "us-east-1"))
  
  (resource s3-bucket "data-bucket"
    (bucket-name "my-data-bucket")
    (versioning #t)
    (encryption "AES256"))
  
  (resource lambda "processor"
    (function-name "data-processor")
    (runtime "python3.9")
    (handler "main.handler")
    (code-uri "./lambda/")
    (environment
      (BUCKET (ref s3-bucket "data-bucket" "name"))))
  
  (output "bucket-arn"
    (value (ref s3-bucket "data-bucket" "arn"))))
```

### 3.2 Variable System
```scheme
(define-variables
  (environment 
    (type string)
    (default "development"))
  (instance-count
    (type number)
    (default 1)))
```

### 3.3 Modules & Composition
```scheme
(use-module "./modules/networking.nim")
(use-module "./modules/compute.nim")

(define-infrastructure main
  (include networking
    (vpc-cidr "10.0.0.0/16"))
  (include compute
    (instance-type "t3.micro")))
```

## Phase 4: Advanced Features (v0.5.0)
**Goal**: Production-ready features

### 4.1 Provider Plugin System
```scheme
(define-provider-interface
  (create-resource)
  (read-resource)
  (update-resource)
  (delete-resource)
  (validate-config))
```

### 4.2 Policy as Code
```scheme
(define-policy production-guardrails
  (deny "s3:DeleteBucket"
    (when (tag-equals "Environment" "Production")))
  (require-approval
    (when (cost-exceeds 1000))))
```

### 4.3 Workspace Management
```bash
nimbus workspace new staging
nimbus workspace select production
nimbus workspace list
```

### 4.4 Remote State
- S3 backend with DynamoDB locking
- State encryption
- Collaborative features

## Phase 5: Enterprise Features (v1.0.0)
**Goal**: Enterprise-grade platform

### 5.1 Security
- Secret rotation automation
- Compliance scanning
- Audit logging
- RBAC with SAML/OIDC

### 5.2 Observability
```scheme
(define-metrics
  (resource-creation-time)
  (deployment-success-rate)
  (drift-detection-results))

(define-alerts
  (on-failure notify-slack)
  (on-drift auto-remediate))
```

### 5.3 Cost Management
- Cost estimation before apply
- Budget alerts
- Resource tagging enforcement
- Usage reports

### 5.4 GitOps Integration
```yaml
# .github/workflows/nimbus.yml
- name: Nimbus Plan
  run: nimbus plan -out=plan.out
- name: Nimbus Apply
  if: github.ref == 'refs/heads/main'
  run: nimbus apply plan.out
```

## Phase 6: Ecosystem (v1.1.0+)
**Goal**: Community and extensibility

### 6.1 Provider Registry
```bash
nimbus provider install aws
nimbus provider install kubernetes
nimbus provider install terraform-compat
```

### 6.2 Module Registry
```bash
nimbus module publish ./my-module
nimbus module install vpc@1.2.0
```

### 6.3 Web UI Dashboard
- Resource visualization
- Real-time status
- Cost tracking
- Team collaboration

### 6.4 IDE Support
- VS Code extension
- Emacs mode
- Language server protocol
- Syntax highlighting

## Technical Improvements Needed

### Performance
- [ ] Lazy evaluation for large state files
- [ ] Parallel resource operations
- [ ] Caching layer for provider API calls
- [ ] Incremental state updates

### Testing
- [ ] Integration tests with LocalStack
- [ ] Property-based testing for DSL
- [ ] Chaos engineering tests
- [ ] Performance benchmarks

### Documentation
- [ ] User guide with tutorials
- [ ] Provider development guide
- [ ] API reference
- [ ] Architecture deep-dives

### Quality of Life
- [ ] Auto-completion for CLI
- [ ] Interactive mode for debugging
- [ ] Migration tool from Terraform
- [ ] Resource graph visualization

## Release Timeline

| Version | Target Date | Key Features |
|---------|------------|--------------|
| v0.2.0  | Q1 2025 | LocalStack provider, Basic CLI |
| v0.3.0  | Q2 2025 | DAG executor, State management |
| v0.4.0  | Q3 2025 | DSL, Modules, Variables |
| v0.5.0  | Q4 2025 | Plugins, Policies, Workspaces |
| v1.0.0  | Q1 2026 | Enterprise features, Security |
| v1.1.0  | Q2 2026 | Registry, Web UI, IDE support |

## Success Metrics

- **Adoption**: 1000+ GitHub stars
- **Community**: 50+ contributors
- **Providers**: 10+ official providers
- **Performance**: <1s for 100 resource plan
- **Reliability**: 99.9% success rate
- **Documentation**: 100% API coverage

## Competitive Advantages

1. **Scheme Power**: Macros for DSL extensibility
2. **Lightweight**: ~10MB binary vs 100MB+ alternatives
3. **LocalStack First**: Optimized for local development
4. **REPL-Driven**: Interactive infrastructure development
5. **Functional**: Immutable state, pure functions
6. **Fast**: Compiled Guile is fast
7. **Extensible**: Lisp's metaprogramming capabilities

## Next Immediate Steps

1. **Create nimbus CLI** with basic commands
2. **Implement S3 provider** for LocalStack
3. **Build simple DSL parser** for .nim files
4. **Add SQLite backend** for state storage
5. **Create example projects** to showcase usage

This roadmap would transform Nimbus from a proof-of-concept into a production-ready IAC platform that could compete with Terraform/Pulumi while offering unique advantages through Guile Scheme's power.