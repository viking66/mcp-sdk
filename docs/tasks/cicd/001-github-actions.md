# Task: Implement Basic GitHub Actions

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages
**Prerequisites**: 
- Development scripts (devenv/004-dev-scripts)

## Context
We need to set up basic GitHub Actions workflows for the MCP SDK project to automate building, testing, and quality checks. These workflows should ensure code quality and project stability across all changes.

## Requirements
1. Configure build workflow
2. Implement test workflow
3. Create quality checks
4. Support dependency caching
5. Enable PR validation

## Detailed Implementation Plan

### 1. Build Workflow

```yaml
# .github/workflows/build.yml
name: Build

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    name: Build on GHC ${{ matrix.ghc }}
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest]
        ghc: ['9.6.6']
        cabal: ['latest']

    steps:
    - uses: actions/checkout@v3

    - uses: haskell-actions/setup@v2
      with:
        ghc-version: ${{ matrix.ghc }}
        cabal-version: ${{ matrix.cabal }}

    - name: Cache
      uses: actions/cache@v3
      with:
        path: |
          ~/.cabal/packages
          ~/.cabal/store
          dist-newstyle
        key: cabal-${{ matrix.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project') }}

    - name: Build
      run: |
        cabal update
        cabal build all --enable-tests --enable-benchmarks
```

### 2. Test Workflow

```yaml
# .github/workflows/test.yml
name: Test

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    name: Test on GHC ${{ matrix.ghc }}
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest]
        ghc: ['9.6.6']
        cabal: ['latest']

    steps:
    - uses: actions/checkout@v3

    - uses: haskell-actions/setup@v2
      with:
        ghc-version: ${{ matrix.ghc }}
        cabal-version: ${{ matrix.cabal }}

    - name: Cache
      uses: actions/cache@v3
      with:
        path: |
          ~/.cabal/packages
          ~/.cabal/store
          dist-newstyle
        key: cabal-test-${{ matrix.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project') }}

    - name: Test
      run: |
        cabal update
        cabal test all
```

### 3. Quality Checks Workflow

```yaml
# .github/workflows/checks.yml
name: Quality Checks

on:
  pull_request:
    branches: [ main ]

jobs:
  checks:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.6.6'

    - name: Install Tools
      run: |
        cabal update
        cabal install hlint
        cabal install fourmolu
        cabal install doctest

    - name: Format Check
      run: |
        find . -name "*.hs" -type f -exec fourmolu --mode check {} +

    - name: Lint Check
      run: |
        hlint .

    - name: Doc Check
      run: |
        ./scripts/check/doc-check.sh
```

### 4. PR Validation Workflow

```yaml
# .github/workflows/pr-validation.yml
name: PR Validation

on:
  pull_request:
    types: [opened, synchronize, reopened]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Validate PR Title
      uses: amannn/action-semantic-pull-request@v5
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}

    - name: Check File Size
      run: |
        ./scripts/check/check-file-size.sh

    - name: Check Dependencies
      run: |
        ./scripts/check/check-deps.sh
```

## Files to Create/Modify
1. Create all workflow files under `.github/workflows/`
2. Add workflow documentation
3. Create workflow scripts
4. Update README.md with badges
5. Add CI configuration files

## Testing Requirements

1. Workflow Tests:
   - Build success
   - Test execution
   - Check reporting

2. Integration Tests:
   - PR process
   - Badge updates
   - Cache usage

3. Performance Tests:
   - Build time
   - Cache effectiveness
   - Resource usage

## Dependencies
```yaml
dependencies:
  - actions/checkout@v3
  - haskell-actions/setup@v2
  - actions/cache@v3
```

## Acceptance Criteria
1. All workflows working
2. Caching effective
3. Checks passing
4. PR validation working
5. Documentation updated
6. Resource efficient
7. Clear feedback
8. Fast execution

## Non-Goals
1. Complex deployments
2. Release automation
3. Advanced monitoring
4. Custom runners

## Resources
1. GitHub Actions Documentation
2. CI Best Practices
3. Workflow Examples