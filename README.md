# Nimbus IAC Platform

[![Status](https://img.shields.io/badge/Status-Experimental-yellow.svg)](https://github.com/dsp-dr/guile-nimbus)
[![Guile 3.0+](https://img.shields.io/badge/Guile-3.0%2B-blue)](https://www.gnu.org/software/guile/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![LocalStack](https://img.shields.io/badge/platform-LocalStack-orange)](https://localstack.cloud/)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)

> **A lightweight, functional Infrastructure as Code platform written in Guile Scheme**

Nimbus brings the power of Lisp to infrastructure management, offering a unique approach to Infrastructure as Code with LocalStack integration, REPL-driven development, and functional programming paradigms.

## Why Nimbus?

- **REPL-Driven Development** - Interactive infrastructure development and debugging
- **Lightweight** - Pure Guile Scheme, no heavy runtimes or large binaries
- **Functional Approach** - Immutable state, pure functions, no side effects
- **LocalStack First** - Optimized for local development and testing
- **Scheme Power** - Macros for DSL extensibility and metaprogramming
- **Transparent** - Simple, readable codebase in elegant Scheme

## Quick Example

```bash
# Initialize a new project
nimbus init my-infrastructure
cd my-infrastructure

# Define infrastructure in infrastructure.nim
cat > infrastructure.nim << 'EOF'
(define-infrastructure my-app
  (provider localstack
    (endpoint "http://localhost:4566"))
  
  (resource s3-bucket "data"
    (versioning #t))
  
  (resource lambda-function "processor"
    (runtime "python3.9")
    (code-uri "./lambda/")))
EOF

# Preview and apply changes
nimbus plan
nimbus apply --auto-approve

# Manage resources
nimbus state list
nimbus destroy
```

## Features

### Core Platform
- **State Management** - Versioned state with atomic operations and rollback snapshots
- **Secrets Management** - Secret model with encryption key rotation (stub crypto, requires guile-gcrypt)
- **Policy Engine** - Rule-based governance with glob pattern matching
- **Deployment History** - Complete audit trail of infrastructure changes
- **Storage Abstraction** - Pluggable backend interface (file-based implementation included)

### Developer Experience  
- **Functional CLI** - Command-line interface with init/plan/apply/destroy workflow
- **Plan & Apply** - Terraform-like preview and execution model
- **LocalStack Provider** - S3 and Lambda resource modules
- **Infrastructure DSL** - Example Scheme-based configuration language (parser not yet implemented)
- **REPL Integration** - Interactive development via Guile REPL

### Current Status (v0.2.0-alpha)
- Core data models (state, secrets, policy, deployment) with GOOPS classes
- LocalStack provider with S3 and Lambda resource modules
- CLI with init, plan, apply, destroy, state, workspace, and validate commands
- File-based storage backend with locking (SQLite bindings not yet integrated)
- 5 test suites using SRFI-64
- CI/CD pipeline on GitHub Actions
- DSL parser and DAG execution engine not yet implemented

## Quick Start

### Prerequisites
- **Guile 3.0+** - Install with your package manager
- **LocalStack** - For infrastructure testing (optional)
- **GNU Make** - For building (gmake on BSD systems)

### 1. Install Dependencies

```bash
# FreeBSD
pkg install guile3 gmake git

# Ubuntu/Debian  
apt-get install guile-3.0 make git

# macOS
brew install guile make git

# Verify installation
guile --version  # Should show 3.0+
```

### 2. Install Nimbus

```bash
# Clone and build
git clone https://github.com/dsp-dr/guile-nimbus.git
cd guile-nimbus

# Check system compatibility  
make check-deps

# Compile
make compile

# Test installation
./bin/nimbus --version
```

### 3. Optional: Install LocalStack

```bash
# Using pip
pip install localstack

# Start LocalStack (in another terminal)
localstack start

# Verify LocalStack is running
curl http://localhost:4566/health
```

### 4. Create Your First Project

```bash
# Initialize project
mkdir my-app && cd my-app
/path/to/guile-nimbus/bin/nimbus init

# Start building!
nimbus plan
```

## Documentation

- **[Quick Start Guide](docs/QUICK_START.md)** - Get up and running in 5 minutes
- **[Development Guide](docs/DEVELOPMENT.md)** - Comprehensive development documentation  
- **[Contributing Guide](CONTRIBUTING.md)** - How to contribute to the project
- **[Architecture Overview](docs/ARCHITECTURE.md)** - Technical architecture and design
- **[Example Projects](examples/)** - Real-world usage examples

## The Nimbus Advantage

### Why Choose Nimbus Over Terraform/Pulumi?

| Feature | Nimbus | Terraform | Pulumi |
|---------|--------|-----------|--------|
| **Runtime** | Guile Scheme (interpreted) | Go binary (~100MB+) | Multi-language (~200MB+) |
| **Language** | Scheme (Functional) | HCL (Declarative) | Multi-language |
| **REPL Support** | Interactive | Static | Limited |
| **LocalStack First** | Optimized | Community | Community |
| **Macros/Metaprogramming** | Powerful | Limited | Language-dependent |

### REPL-Driven Infrastructure Development

```scheme
;; Start interactive session
$ nimbus repl

;; Develop infrastructure interactively
scheme@(nimbus)> (define my-bucket (make-s3-bucket "test"))
scheme@(nimbus)> (preview-creation my-bucket)
scheme@(nimbus)> (create-resource my-bucket)
scheme@(nimbus)> (inspect-resource my-bucket)

;; Test and iterate instantly
scheme@(nimbus)> (modify-bucket my-bucket #:versioning #t)
scheme@(nimbus)> (apply-changes)
```

## Community & Contributing

We welcome contributions from developers of all skill levels! Whether you're a Scheme expert or just getting started, there are many ways to help.

### Ways to Contribute

- **Report Bugs** - Found something broken? [Open an issue](https://github.com/dsp-dr/guile-nimbus/issues)
- **Suggest Features** - Have ideas? [Start a discussion](https://github.com/dsp-dr/guile-nimbus/discussions)  
- **Improve Docs** - Help make our documentation better
- **Write Code** - Implement new features or fix bugs
- **Add Tests** - Help improve our test coverage
- **Create Examples** - Show how to use Nimbus in real projects

### Good First Issues

Looking to get started? Check out issues labeled [`good first issue`](https://github.com/dsp-dr/guile-nimbus/labels/good%20first%20issue):

- Add more LocalStack resources (SNS, SQS, DynamoDB, etc.)
- Improve CLI help messages and error handling
- Write tutorials and examples
- Add command autocompletion
- Improve code documentation

### Development Workflow

```bash
# Fork and clone
git clone https://github.com/YOUR-USERNAME/guile-nimbus.git
cd guile-nimbus

# Create feature branch  
git checkout -b feature/amazing-feature

# Make changes, add tests
make test

# Submit pull request
git push origin feature/amazing-feature
```

See our [Contributing Guide](CONTRIBUTING.md) for detailed instructions.

## Roadmap

### Current Phase: Core Platform (v0.2.0)
- [x] LocalStack provider with S3 and Lambda
- [x] CLI with plan/apply workflow  
- [x] File-based storage backend with locking
- [x] Test suites (5 suites, SRFI-64)

### Next Phase: DSL & Execution (v0.3.0)
- [ ] Infrastructure DSL parser
- [ ] DAG-based dependency resolution
- [ ] SQLite storage backend (via guile-sqlite3 bindings)
- [ ] Real AWS API integration
- [ ] Resource import/export

### Future Phases: Enterprise & Ecosystem (v0.4.0+)
- [ ] Remote state backends (S3, PostgreSQL)
- [ ] Actual crypto for secrets (via guile-gcrypt)
- [ ] Web UI and visualization  
- [ ] Multi-cloud providers (AWS, Azure, GCP)
- [ ] Terraform compatibility layer

See our detailed [Roadmap](ROADMAP.md) for more information.

## Project Status

- **Alpha Stage** - Core data models working, provider integration stubbed, API may change
- **Active Development** - Regular commits and releases  
- **Community Building** - Seeking contributors and feedback
- **CI/CD Pipeline** - Automated testing on GitHub Actions

## Project Structure

```
guile-nimbus/
├── nimbus/
│   ├── cli/
│   │   └── commands.scm          # CLI command implementations
│   ├── core/
│   │   ├── config.scm            # Configuration management
│   │   └── plan.scm              # Execution plan calculation
│   ├── models/
│   │   ├── state.scm             # State management
│   │   ├── secrets.scm           # Secrets handling
│   │   ├── policy.scm            # Policy engine
│   │   └── deployment.scm        # Deployment tracking
│   ├── providers/
│   │   └── localstack/
│   │       ├── provider.scm      # LocalStack provider
│   │       ├── s3.scm            # S3 bucket resources
│   │       └── lambda.scm        # Lambda function resources
│   └── storage/
│       ├── interface.scm         # Storage backend interface
│       └── sqlite.scm            # File-based storage backend
├── bin/
│   └── nimbus                    # CLI entry point
├── tests/                        # SRFI-64 test suites
├── examples/                     # Example infrastructure definitions
├── docs/                         # Documentation
├── Makefile                      # Build configuration
└── README.md                     # This file
```

## Core Components

### State Management
- Resource tracking with dependencies (GOOPS classes)
- State snapshots for rollback capability
- Checksums for integrity verification (simple hash fallback)

### Secrets Management
- Encryption key and secret models with GOOPS
- Key rotation support (key lifecycle management)
- Access audit trail records
- Crypto operations stubbed (requires guile-gcrypt for real AES-256)

### Policy Engine
- Glob-based pattern matching for actions and resources
- Allow/Deny effects with conditional evaluation
- Policy binding and evaluation records

### Deployment History
- Step-by-step execution tracking with status lifecycle
- Artifact management
- Rollback support via status tracking

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