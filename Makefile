# Nimbus IAC Platform - Makefile
# Infrastructure as Code platform for LocalStack

# Variables - Try to find Guile 3
# First try guile3/guild3, then fallback to guile/guild if version 3+
GUILE := $(shell command -v guile3 2>/dev/null || command -v guile 2>/dev/null)
GUILD := $(shell command -v guild3 2>/dev/null || command -v guild 2>/dev/null)

# Get version info
GUILE_VERSION := $(shell $(GUILE) --version 2>/dev/null | head -n 1 | awk '{print $$NF}')
GUILE_MAJOR_VERSION := $(shell $(GUILE) --version 2>/dev/null | head -n 1 | awk '{print $$NF}' | cut -d. -f1)
GUILE_MINOR_VERSION := $(shell $(GUILE) --version 2>/dev/null | head -n 1 | awk '{print $$NF}' | cut -d. -f2)

# Enforce Guile 3.0 or higher
GUILE_VERSION_OK := $(shell [ $(GUILE_MAJOR_VERSION) -ge 3 ] && echo yes || echo no)

PREFIX := /usr/local
LIBDIR := $(PREFIX)/lib/guile/$(GUILE_VERSION)/site-ccache
DATADIR := $(PREFIX)/share/guile/site/$(GUILE_VERSION)
DOCDIR := $(PREFIX)/share/doc/nimbus

# Source files
SOURCES := $(shell find nimbus -name '*.scm' -type f)
COMPILED := $(SOURCES:.scm=.go)

# Test files
TEST_SOURCES := $(shell find tests -name '*.scm' -type f 2>/dev/null || echo "")

# Documentation
DOCS := README.md docs/*.md

# Default target
.PHONY: all
all: check-version compile

# Version check
.PHONY: check-version
check-version:
	@if [ -z "$(GUILE)" ]; then \
		echo "Error: Guile not found. Please install Guile 3.0 or higher"; \
		echo "  Ubuntu/Debian: apt-get install guile-3.0"; \
		echo "  FreeBSD: pkg install guile3"; \
		echo "  macOS: brew install guile"; \
		exit 1; \
	fi
	@if [ "$(GUILE_VERSION_OK)" != "yes" ]; then \
		echo "Error: Guile 3.0 or higher is required. Found version $(GUILE_VERSION)"; \
		echo "Please install Guile 3 (e.g., 'pkg install guile3' on FreeBSD)"; \
		exit 1; \
	fi
	@echo "Guile version check passed: $(GUILE_VERSION)"

# Compilation
.PHONY: compile
compile: check-version $(COMPILED)

%.go: %.scm
	@echo "Compiling $<..."
	@mkdir -p $(dir $@)
	$(GUILD) compile \
		-Wunsupported-warning \
		-Wunused-variable \
		-Wunused-toplevel \
		-Wunbound-variable \
		-o $@ $<

# Clean compiled files
.PHONY: clean
clean:
	@echo "Cleaning compiled files..."
	@find . -name '*.go' -delete
	@find . -name '*~' -delete

# Installation
.PHONY: install
install: compile
	@echo "Installing Nimbus to $(PREFIX)..."
	@mkdir -p $(LIBDIR)/nimbus
	@mkdir -p $(DATADIR)/nimbus
	@mkdir -p $(DOCDIR)
	@cp -r nimbus/*.scm $(DATADIR)/nimbus/
	@cp -r nimbus/*.go $(LIBDIR)/nimbus/
	@cp -r docs/* $(DOCDIR)/ 2>/dev/null || true
	@cp README.md $(DOCDIR)/
	@echo "Installation complete!"

# Uninstallation
.PHONY: uninstall
uninstall:
	@echo "Uninstalling Nimbus from $(PREFIX)..."
	@rm -rf $(LIBDIR)/nimbus
	@rm -rf $(DATADIR)/nimbus
	@rm -rf $(DOCDIR)
	@echo "Uninstallation complete!"

# Testing
.PHONY: test
test: compile
	@echo "Running tests..."
	@if [ -n "$(TEST_SOURCES)" ]; then \
		for test in $(TEST_SOURCES); do \
			echo "Running $$test..."; \
			$(GUILE) -L . $$test; \
		done; \
	else \
		echo "No tests found in tests/ directory"; \
	fi

# Check dependencies
.PHONY: check-deps
check-deps: check-version
	@echo "Checking dependencies..."
	@echo "Guile version: $(GUILE_VERSION) (Major: $(GUILE_MAJOR_VERSION), Minor: $(GUILE_MINOR_VERSION))"
	@$(GUILE) -c "(use-modules (oop goops)) (display \"GOOPS: OK\n\")"
	@$(GUILE) -c "(use-modules (srfi srfi-1)) (display \"SRFI-1: OK\n\")"
	@$(GUILE) -c "(use-modules (srfi srfi-19)) (display \"SRFI-19: OK\n\")"
	@$(GUILE) -c "(use-modules (ice-9 hash-table)) (display \"Hash tables: OK\n\")"
	@$(GUILE) -c "(use-modules (ice-9 regex)) (display \"Regex: OK\n\")"
	@$(GUILE) -c "(use-modules (json)) (display \"JSON: OK\n\")" 2>/dev/null || \
		echo "JSON: MISSING (install guile3-json or guile-json)"
	@$(GUILE) -c "(use-modules (gcrypt hash)) (display \"Gcrypt hash: OK\n\")" 2>/dev/null || \
		echo "Gcrypt: MISSING (install guile3-gcrypt or guile-gcrypt)"
	@$(GUILE) -c "(use-modules (gcrypt cipher)) (display \"Gcrypt cipher: OK\n\")" 2>/dev/null || \
		echo "Gcrypt cipher: MISSING (install guile3-gcrypt or guile-gcrypt)"
	@$(GUILE) -c "(use-modules (gcrypt base64)) (display \"Gcrypt base64: OK\n\")" 2>/dev/null || \
		echo "Gcrypt base64: MISSING (install guile3-gcrypt or guile-gcrypt)"

# REPL for development
.PHONY: repl
repl:
	@echo "Starting Guile REPL with Nimbus modules..."
	$(GUILE) -L . --listen

# Documentation generation (if using texinfo)
.PHONY: docs
docs:
	@echo "Generating documentation..."
	@if command -v makeinfo >/dev/null 2>&1; then \
		if [ -f docs/nimbus.texi ]; then \
			makeinfo --html --output=docs/html docs/nimbus.texi; \
			echo "HTML documentation generated in docs/html/"; \
		else \
			echo "No texinfo source found"; \
		fi; \
	else \
		echo "makeinfo not found, skipping documentation generation"; \
	fi

# Format check (basic style checking)
.PHONY: check-style
check-style:
	@echo "Checking code style..."
	@for file in $(SOURCES); do \
		echo -n "Checking $$file... "; \
		if grep -q '	' $$file; then \
			echo "FAIL: contains tabs"; \
		else \
			echo "OK"; \
		fi; \
	done

# Development helpers
.PHONY: dev
dev: clean compile test

# Watch for changes and recompile (requires inotify-tools)
.PHONY: watch
watch:
	@echo "Watching for changes..."
	@if command -v inotifywait >/dev/null 2>&1; then \
		while true; do \
			inotifywait -qr -e modify,create,delete nimbus/; \
			$(MAKE) compile; \
		done; \
	else \
		echo "inotifywait not found. Install inotify-tools for file watching."; \
		exit 1; \
	fi

# Package for distribution
.PHONY: dist
dist: clean
	@echo "Creating distribution package..."
	@mkdir -p dist
	@tar czf dist/nimbus-$(shell date +%Y%m%d).tar.gz \
		--exclude=dist \
		--exclude=.git \
		--exclude=*.go \
		Makefile README.md nimbus/ tests/ docs/
	@echo "Distribution package created in dist/"

# Help
.PHONY: help
help:
	@echo "Nimbus IAC Platform - Available targets:"
	@echo ""
	@echo "  all          - Compile all Scheme files (default)"
	@echo "  compile      - Compile all Scheme files to bytecode"
	@echo "  clean        - Remove compiled files"
	@echo "  install      - Install Nimbus to system"
	@echo "  uninstall    - Remove Nimbus from system"
	@echo "  test         - Run test suite"
	@echo "  check-deps   - Check for required dependencies"
	@echo "  repl         - Start interactive REPL with modules loaded"
	@echo "  docs         - Generate documentation"
	@echo "  check-style  - Basic style checking"
	@echo "  dev          - Clean, compile, and test"
	@echo "  watch        - Watch files and auto-recompile"
	@echo "  dist         - Create distribution package"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Configuration:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  GUILE=$(GUILE)"
	@echo "  GUILE_VERSION=$(GUILE_VERSION)"

# Declare all targets as phony except for pattern rules
.PHONY: all compile clean install uninstall test check-deps repl docs check-style dev watch dist help