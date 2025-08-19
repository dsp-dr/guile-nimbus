# Nimbus IAC Platform

[![Guile Version](https://img.shields.io/badge/Guile-3.0%2B-blue)](https://www.gnu.org/software/guile/)
[![License](https://img.shields.io/github/license/dsp-dr/guile-nimbus)](LICENSE)
[![Code Style](https://img.shields.io/badge/code%20style-scheme-green)](https://www.scheme.org/)
[![Platform](https://img.shields.io/badge/platform-LocalStack-orange)](https://localstack.cloud/)
[![Build Status](https://img.shields.io/github/actions/workflow/status/dsp-dr/guile-nimbus/ci.yml?branch=main)](https://github.com/dsp-dr/guile-nimbus/actions)
[![Issues](https://img.shields.io/github/issues/dsp-dr/guile-nimbus)](https://github.com/dsp-dr/guile-nimbus/issues)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Made with Guile](https://img.shields.io/badge/Made%20with-Guile%20Scheme-blue)](https://www.gnu.org/software/guile/)

A lightweight Infrastructure as Code (IaC) platform written in Guile Scheme, targeting LocalStack for rapid cloud infrastructure development.

## Features

- **State Management**: Versioned state tracking with snapshots for rollback
- **Secrets Management**: Encrypted secrets with audit trails and key rotation
- **Policy Engine**: Flexible policy system with pattern matching and rule evaluation
- **Deployment History**: Comprehensive deployment tracking with step-by-step execution
- **Storage Abstraction**: Pluggable storage backend interface

## Requirements

- **Guile 3.0** or higher (enforced by Makefile)
- **guile3-json** for JSON support
- **guile3-gcrypt** for encryption and hashing
- **GNU Make** (gmake on BSD systems)

### Installing Dependencies

#### FreeBSD
```bash
pkg install guile3 guile3-json guile3-gcrypt gmake
```

#### Debian/Ubuntu
```bash
apt-get install guile-3.0 guile-json guile-gcrypt make
```

#### macOS (Homebrew)
```bash
brew install guile guile-json
# Note: guile-gcrypt may need to be built from source
```

## Installation

### Quick Start
```bash
# Check dependencies
gmake check-deps

# Compile the project
gmake compile

# Run tests
gmake test

# Install system-wide
sudo gmake install
```

### Development Setup
```bash
# Clone the repository
git clone https://github.com/dsp-dr/guile-nimbus.git
cd guile-nimbus

# Start development REPL
gmake repl

# Watch mode for auto-compilation
gmake watch
```

## Usage

### Basic Example
```scheme
(use-modules (nimbus models state)
             (nimbus models deployment))

;; Create a new state
(define my-state 
  (make-state #:id "prod-stack-001"
              #:stack-name "production"
              #:environment "prod"))

;; Add a resource
(define resource 
  (make-resource #:id "s3-bucket-001"
                 #:state-id "prod-stack-001"
                 #:resource-type "AWS::S3::Bucket"
                 #:resource-name "my-data-bucket"))

(add-resource my-state resource)
```

## Project Structure

```
guile-nimbus/
├── nimbus/
│   ├── models/
│   │   ├── state.scm         # State management
│   │   ├── secrets.scm       # Secrets handling
│   │   ├── policy.scm        # Policy engine
│   │   └── deployment.scm    # Deployment tracking
│   ├── storage/
│   │   └── interface.scm     # Storage backend interface
│   └── utils/               # Utility modules
├── tests/                   # Test suite
├── docs/                    # Documentation
├── Makefile                # Build configuration
└── README.md              # This file
```

## Core Components

### State Management
- Resource tracking with dependencies
- State snapshots for rollback capability
- Checksums for integrity verification

### Secrets Management
- AES-256 encryption
- Key rotation support
- Access audit trails

### Policy Engine
- Pattern-based rule matching
- Allow/Deny effects
- Conditional evaluation

### Deployment History
- Step-by-step execution tracking
- Artifact management
- Rollback support

## Development

### Make Targets

| Target | Description |
|--------|-------------|
| `all` | Default target, compiles all files |
| `compile` | Compile Scheme files to bytecode |
| `clean` | Remove compiled files |
| `test` | Run test suite |
| `check-deps` | Check for required dependencies |
| `check-version` | Verify Guile 3.0+ requirement |
| `repl` | Start interactive REPL |
| `watch` | Auto-recompile on file changes |
| `dist` | Create distribution package |
| `install` | Install to system |
| `uninstall` | Remove from system |

### Code Style

The project follows standard Scheme conventions:
- No tabs in source files
- Consistent indentation
- GOOPS for object-oriented programming
- SRFI libraries for standard functionality

### Running Tests
```bash
# Run all tests
gmake test

# Run specific test file
guile3 -L . tests/test-state.scm
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## Roadmap

- [ ] SQLite storage backend implementation
- [ ] DynamoDB storage backend for AWS
- [ ] LocalStack resource providers
- [ ] DAG-based execution engine
- [ ] CLI interface
- [ ] Web UI dashboard
- [ ] Terraform compatibility layer

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

- **Issues**: [GitHub Issues](https://github.com/dsp-dr/guile-nimbus/issues)
- **Discussions**: [GitHub Discussions](https://github.com/dsp-dr/guile-nimbus/discussions)
- **Security**: Report security vulnerabilities privately via GitHub Security Advisories

## Acknowledgments

- Built with [GNU Guile](https://www.gnu.org/software/guile/)
- Inspired by Terraform and Pulumi
- Designed for [LocalStack](https://localstack.cloud/)

---

**Note**: This project requires Guile 3.0 or higher. The build system will enforce this requirement.