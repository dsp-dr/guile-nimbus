#!/bin/sh
# Test runner script for Nimbus IAC Platform

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
GUILE=${GUILE:-guile3}
MAKE=${MAKE:-gmake}

echo "========================================="
echo "     Nimbus IAC Platform Test Suite     "
echo "========================================="
echo ""

# Check Guile version
echo "Checking Guile version..."
if ! $GUILE --version > /dev/null 2>&1; then
    echo "${RED}Error: Guile not found. Please install Guile 3.0 or higher.${NC}"
    exit 1
fi

GUILE_VERSION=$($GUILE --version | head -n1 | awk '{print $NF}')
GUILE_MAJOR=$(echo $GUILE_VERSION | cut -d. -f1)

if [ "$GUILE_MAJOR" -lt 3 ]; then
    echo "${RED}Error: Guile 3.0 or higher required. Found version $GUILE_VERSION${NC}"
    exit 1
fi

echo "${GREEN}✓ Guile version $GUILE_VERSION${NC}"
echo ""

# Clean previous build artifacts
echo "Cleaning previous build artifacts..."
$MAKE clean > /dev/null 2>&1
echo "${GREEN}✓ Clean complete${NC}"
echo ""

# Compile modules
echo "Compiling modules..."
if $MAKE compile > compile.log 2>&1; then
    COMPILED=$(grep -c "wrote" compile.log || echo 0)
    echo "${GREEN}✓ Compiled $COMPILED modules successfully${NC}"
else
    echo "${RED}✗ Compilation failed. Check compile.log for details.${NC}"
    tail -20 compile.log
    exit 1
fi
echo ""

# Run tests
echo "Running test suite..."
echo "----------------------------------------"

TOTAL_PASS=0
TOTAL_FAIL=0
FAILED_TESTS=""

for test in tests/test-*.scm; do
    if [ -f "$test" ]; then
        TEST_NAME=$(basename "$test" .scm | sed 's/test-//')
        printf "Testing %-20s ... " "$TEST_NAME"
        
        if $GUILE -L . "$test" > "${TEST_NAME}.test.log" 2>&1; then
            PASSES=$(grep "# of expected passes" "${TEST_NAME}.test.log" | awk '{print $5}')
            FAILURES=$(grep "# of unexpected failures" "${TEST_NAME}.test.log" | awk '{print $5}' || echo 0)
            
            if [ -n "$PASSES" ]; then
                TOTAL_PASS=$((TOTAL_PASS + PASSES))
            fi
            
            if [ -n "$FAILURES" ] && [ "$FAILURES" -gt 0 ]; then
                TOTAL_FAIL=$((TOTAL_FAIL + FAILURES))
                FAILED_TESTS="$FAILED_TESTS $TEST_NAME"
                echo "${YELLOW}⚠ $PASSES passed, $FAILURES failed${NC}"
            else
                echo "${GREEN}✓ $PASSES passed${NC}"
            fi
        else
            echo "${RED}✗ ERROR${NC}"
            FAILED_TESTS="$FAILED_TESTS $TEST_NAME"
            tail -5 "${TEST_NAME}.test.log"
        fi
    fi
done

echo "----------------------------------------"
echo ""

# Summary
echo "Test Summary"
echo "============"
echo "${GREEN}Total tests passed: $TOTAL_PASS${NC}"

if [ "$TOTAL_FAIL" -gt 0 ]; then
    echo "${RED}Total tests failed: $TOTAL_FAIL${NC}"
    echo ""
    echo "Failed test suites:$FAILED_TESTS"
    exit 1
fi

if [ -n "$FAILED_TESTS" ]; then
    echo "${RED}Test suites with errors:$FAILED_TESTS${NC}"
    exit 1
fi

echo ""
echo "${GREEN}All tests passed successfully! 🎉${NC}"

# Cleanup test logs unless KEEP_LOGS is set
if [ -z "$KEEP_LOGS" ]; then
    rm -f *.test.log compile.log
fi

exit 0