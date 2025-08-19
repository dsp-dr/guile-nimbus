# Contributing to Nimbus IAC Platform

Thank you for your interest in contributing to Nimbus! This document provides guidelines and instructions for contributing to the project.

## Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct:
- Be respectful and inclusive
- Welcome newcomers and help them get started
- Focus on constructive criticism
- Accept responsibility for mistakes

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/guile-nimbus.git
   cd guile-nimbus
   ```
3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/dsp-dr/guile-nimbus.git
   ```
4. **Install dependencies**:
   ```bash
   # FreeBSD
   pkg install guile3 guile3-json guile3-gcrypt gmake
   
   # Debian/Ubuntu
   apt-get install guile-3.0 guile-json guile-gcrypt make
   ```

## Development Workflow

### 1. Create a Feature Branch
```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-bugfix-name
```

### 2. Make Your Changes
- Follow the existing code style
- Write clear, concise commit messages
- Add tests for new functionality
- Update documentation as needed

### 3. Test Your Changes
```bash
# Check dependencies
gmake check-deps

# Compile
gmake compile

# Run tests
gmake test

# Check code style
gmake check-style
```

### 4. Commit Your Changes
Use conventional commit format:
```bash
git commit -m "feat: add new feature description"
git commit -m "fix: resolve specific issue"
git commit -m "docs: update documentation"
git commit -m "test: add test cases"
git commit -m "refactor: improve code structure"
```

### 5. Push and Create Pull Request
```bash
git push origin feature/your-feature-name
```
Then create a pull request on GitHub.

## Coding Standards

### Guile Scheme Style Guide

1. **Indentation**: Use spaces, not tabs
2. **Line Length**: Keep lines under 80 characters when possible
3. **Naming Conventions**:
   - Functions: `lowercase-with-hyphens`
   - Constants: `+constant-name+`
   - Classes (GOOPS): `<class-name>`
   - Predicates: `name?`
   - Destructive operations: `name!`

4. **Module Structure**:
```scheme
;;; -*- mode: scheme; -*-
;;; module-name.scm - Brief description

(define-module (nimbus category module-name)
  #:use-module (...)
  #:export (...))

;; Implementation
```

5. **Documentation**:
   - Add docstrings to all public functions
   - Include usage examples in comments
   - Update README for user-facing changes

### Testing Guidelines

1. **Test File Naming**: `tests/test-module-name.scm`
2. **Test Structure**:
```scheme
(use-modules (srfi srfi-64)
             (nimbus module-name))

(test-begin "module-name")

(test-equal "description of test"
  expected-value
  (function-to-test args))

(test-end "module-name")
```

3. **Coverage**: Aim for >80% test coverage
4. **Edge Cases**: Test boundary conditions and error cases

## Pull Request Process

1. **Before Submitting**:
   - Ensure all tests pass
   - Update documentation
   - Add yourself to CONTRIBUTORS if first contribution
   - Rebase on latest main branch

2. **PR Description Should Include**:
   - Summary of changes
   - Related issue numbers (fixes #123)
   - Testing performed
   - Breaking changes (if any)

3. **Review Process**:
   - PRs require at least one review
   - Address all feedback constructively
   - Keep PRs focused and atomic

## Reporting Issues

### Bug Reports Should Include:
- Guile version (`guile3 --version`)
- Operating system
- Steps to reproduce
- Expected vs actual behavior
- Error messages/stack traces

### Feature Requests Should Include:
- Use case description
- Proposed API/interface
- Alternative solutions considered
- Impact on existing functionality

## Documentation

- **Code Comments**: Explain "why" not "what"
- **README Updates**: For new features or changed usage
- **API Documentation**: For public functions/modules
- **Examples**: Add to `examples/` directory

## Release Process

1. Version numbers follow Semantic Versioning (MAJOR.MINOR.PATCH)
2. Maintain CHANGELOG.md with all changes
3. Tag releases with `v` prefix (e.g., `v1.2.3`)
4. Create GitHub release with changelog excerpt

## Getting Help

- **Questions**: Open a GitHub Discussion
- **Bugs**: Open a GitHub Issue
- **Security**: Report privately via GitHub Security Advisories

## Recognition

Contributors will be recognized in:
- CONTRIBUTORS file
- Release notes
- Project documentation

Thank you for contributing to Nimbus!