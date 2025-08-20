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
├── models/          # Core data models
│   ├── state.scm    # State management
│   ├── secrets.scm  # Secrets handling
│   ├── policy.scm   # Policy engine
│   └── deployment.scm # Deployment tracking
└── storage/         # Storage abstraction
    └── interface.scm # Backend interface
```