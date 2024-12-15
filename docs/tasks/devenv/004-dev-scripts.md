# Task: Implement Development Scripts

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages
**Prerequisites**: 
- HLS configuration (001-hls-config)
- Formatter setup (002-formatter-setup)
- Linter configuration (003-linter-config)

## Context
We need to create a set of development scripts to streamline common development tasks, automate quality checks, and provide consistent workflows across the project. These scripts should make it easier for developers to work with the codebase and maintain quality standards.

## Requirements
1. Create build scripts
2. Implement test runners
3. Add quality check scripts
4. Support development tasks
5. Enable CI preparation

## Detailed Implementation Plan

### 1. Core Scripts Structure

```bash
scripts/
├── build/
│   ├── build-all.sh
│   ├── build-core.sh
│   ├── build-server.sh
│   └── build-client.sh
├── test/
│   ├── test-all.sh
│   ├── test-core.sh
│   ├── test-server.sh
│   └── test-client.sh
├── check/
│   ├── check-all.sh
│   ├── format-check.sh
│   ├── lint-check.sh
│   └── doc-check.sh
└── dev/
    ├── setup-env.sh
    ├── clean-all.sh
    ├── watch-tests.sh
    └── update-deps.sh
```

### 2. Build Scripts

```bash
#!/bin/bash
# scripts/build/build-all.sh

set -euo pipefail

# Configuration
GHC_VERSION="9.6.6"
CABAL_FLAGS="--enable-tests --enable-benchmarks"

# Build function
build_package() {
    local package=$1
    echo "Building $package..."
    cabal build $CABAL_FLAGS "mcp-$package"
}

# Build all packages
build_package "core"
build_package "server"
build_package "client"

echo "Build complete!"
```

### 3. Test Scripts

```bash
#!/bin/bash
# scripts/test/test-all.sh

set -euo pipefail

# Configuration
TEST_FLAGS="--test-show-details=direct"

# Test function
run_tests() {
    local package=$1
    echo "Testing $package..."
    cabal test $TEST_FLAGS "mcp-$package"
}

# Run all tests
run_tests "core"
run_tests "server"
run_tests "client"

echo "Tests complete!"
```

### 4. Quality Check Scripts

```bash
#!/bin/bash
# scripts/check/check-all.sh

set -euo pipefail

# Run all checks
echo "Running format check..."
./scripts/check/format-check.sh

echo "Running lint check..."
./scripts/check/lint-check.sh

echo "Running doc check..."
./scripts/check/doc-check.sh

echo "All checks passed!"
```

### 5. Development Helper Scripts

```bash
#!/bin/bash
# scripts/dev/setup-env.sh

set -euo pipefail

# Install dependencies
cabal update
cabal install --only-dependencies --enable-tests all

# Install development tools
cabal install doctest
cabal install hlint
cabal install fourmolu

# Set up git hooks
cp scripts/git-hooks/* .git/hooks/
chmod +x .git/hooks/*

echo "Development environment setup complete!"
```

## Files to Create/Modify
1. Create all script files under `scripts/`
2. Add script documentation
3. Update root README.md
4. Create script test files
5. Update CI configuration

## Testing Requirements

1. Script Tests:
   - Command line arguments
   - Error handling
   - Exit codes

2. Integration Tests:
   - Multi-package builds
   - Test running
   - Check execution

3. Environment Tests:
   - Different OS support
   - Tool availability
   - Permission handling

## Usage Examples

1. Building:
   ```bash
   # Build all packages
   ./scripts/build/build-all.sh
   
   # Build specific package
   ./scripts/build/build-core.sh
   ```

2. Testing:
   ```bash
   # Run all tests
   ./scripts/test/test-all.sh
   
   # Watch tests
   ./scripts/dev/watch-tests.sh mcp-core
   ```

3. Checking:
   ```bash
   # Run all checks
   ./scripts/check/check-all.sh
   
   # Run specific check
   ./scripts/check/format-check.sh
   ```

## Dependencies
```yaml
dependencies:
  - bash >= 4.0
  - cabal >= 3.0
  - ghc-9.6.6
```

## Acceptance Criteria
1. All scripts working
2. Error handling robust
3. Documentation clear
4. Cross-platform support
5. Fast execution
6. Easy to maintain
7. Well documented
8. CI integration working

## Non-Goals
1. Complex build system
2. Deployment scripts
3. Production tooling
4. Package management

## Resources
1. Bash Scripting Guide
2. CI Integration Examples
3. Development Workflow Patterns