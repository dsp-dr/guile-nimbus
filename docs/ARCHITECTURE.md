# Nimbus Architecture

## Overview
Nimbus is a lightweight Infrastructure as Code platform written in Guile Scheme.

## Core Components

### State Management
- Tracks infrastructure resources
- Manages state snapshots for rollback
- Provides checksum verification

### Secrets Management  
- Encrypted storage of sensitive data
- Key rotation support
- Access audit logging

### Policy Engine
- Rule-based access control
- Pattern matching for resources
- Conditional evaluation

### Deployment History
- Step-by-step execution tracking
- Artifact management
- Rollback capabilities

### Storage Backend
- Abstract interface for different storage providers
- Support for transactions and locking
- Pluggable architecture

## Module Structure

```
nimbus/
├── cli/
│   └── commands.scm      # CLI command implementations (init, plan, apply, etc.)
├── core/
│   ├── config.scm        # Configuration loading/saving (nimbus.config files)
│   └── plan.scm          # Execution plan calculation and diffing
├── models/               # Core data models (GOOPS classes)
│   ├── state.scm         # State, Resource, StateSnapshot classes
│   ├── secrets.scm       # EncryptionKey, Secret, SecretAccess classes
│   ├── policy.scm        # Policy, PolicyRule, PolicyBinding classes
│   └── deployment.scm    # Deployment, DeploymentStep, DeploymentArtifact classes
├── providers/
│   └── localstack/
│       ├── provider.scm  # LocalStack provider (endpoint, auth, health check)
│       ├── s3.scm        # S3 bucket CRUD operations
│       └── lambda.scm    # Lambda function CRUD and invoke
└── storage/              # Storage abstraction
    ├── interface.scm     # Abstract backend interface (generics)
    └── sqlite.scm        # File-based storage backend with locking
```