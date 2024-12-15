# Task: Implement Documentation Generation

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages
**Prerequisites**: 
- Basic GitHub Actions (001-github-actions)
- Automated testing (002-automated-testing)

## Context
We need to implement automated documentation generation in our CI pipeline, including Haddock documentation, examples, and user guides. The system should generate and publish documentation automatically on releases and maintain documentation versioning.

## Requirements
1. Configure Haddock generation
2. Implement example compilation
3. Create user guide generation
4. Support documentation testing
5. Enable automated publishing

## Detailed Implementation Plan

### 1. Documentation Generation Workflow

```yaml
# .github/workflows/docs.yml
name: Documentation

on:
  push:
    branches: [ main ]
  release:
    types: [ published ]
  pull_request:
    paths:
      - '**.hs'
      - '**.md'
      - 'docs/**'
      - '.github/workflows/docs.yml'

jobs:
  haddock:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3

    - name: Setup Haskell
      uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.6.6'

    - name: Generate Haddock
      run: |
        cabal haddock all --haddock-all --haddock-hyperlink-source
        ./scripts/docs/post-process-haddock.sh

    - name: Upload Documentation
      uses: actions/upload-pages-artifact@v2
      with:
        path: dist-newstyle/build/*/ghc-*/mcp-*/doc/html/

  examples:
    runs-on: ubuntu-latest
    steps:
    - name: Build Examples
      run: |
        ./scripts/docs/build-examples.sh

    - name: Test Examples
      run: |
        ./scripts/docs/test-examples.sh
```

### 2. Documentation Scripts

```bash
#!/bin/bash
# scripts/docs/generate-docs.sh

set -euo pipefail

# Configuration
DOCS_DIR="docs"
OUTPUT_DIR="generated-docs"
EXAMPLES_DIR="examples"

# Generate all documentation
generate_all_docs() {
    echo "Generating documentation..."
    
    # Haddock
    cabal haddock all \
        --haddock-all \
        --haddock-hyperlink-source \
        --haddock-quickjump \
        --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs'

    # Examples
    ./scripts/docs/build-examples.sh

    # User guide
    ./scripts/docs/build-guide.sh

    # API reference
    ./scripts/docs/build-api-docs.sh
}

# Post-process documentation
post_process_docs() {
    echo "Post-processing documentation..."
    
    # Fix links
    find "$OUTPUT_DIR" -name "*.html" -type f -exec \
        ./scripts/docs/fix-links.sh {} \;

    # Generate search index
    ./scripts/docs/generate-search-index.sh

    # Create version dropdown
    ./scripts/docs/add-version-selector.sh
}
```

### 3. Documentation Testing

```yaml
# .github/workflows/doc-tests.yml
name: Documentation Tests

jobs:
  doctest:
    runs-on: ubuntu-latest
    steps:
    - name: Run Doctest
      run: |
        cabal install doctest
        ./scripts/docs/run-doctests.sh

  example-tests:
    runs-on: ubuntu-latest
    steps:
    - name: Test Examples
      run: |
        ./scripts/docs/test-examples.sh

  markdown-tests:
    runs-on: ubuntu-latest
    steps:
    - name: Check Markdown
      run: |
        npx markdownlint-cli2 "**/*.md"
```

### 4. Publishing Configuration

```yaml
# .github/workflows/publish-docs.yml
name: Publish Documentation

on:
  release:
    types: [published]

jobs:
  deploy:
    runs-on: ubuntu-latest
    permissions:
      pages: write
      id-token: write
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
    - name: Deploy to GitHub Pages
      id: deployment
      uses: actions/deploy-pages@v2
```

## Testing Requirements

1. Documentation Tests:
   - Doctest execution
   - Example compilation
   - Link validation

2. Format Tests:
   - Markdown validation
   - HTML validation
   - Code highlighting

3. Build Tests:
   - Documentation generation
   - Example building
   - Publishing process

## Files to Create/Modify
1. Create workflow files under `.github/workflows/`
2. Create documentation scripts under `scripts/docs/`
3. Update documentation guidelines
4. Add CI configuration files
5. Create publishing configuration

## Dependencies
```yaml
dependencies:
  - actions/deploy-pages@v2
  - haskell-actions/setup@v2
  - markdownlint-cli2
```

## Acceptance Criteria
1. Documentation generates cleanly
2. Examples compile
3. Tests pass
4. Links valid
5. Publishing works
6. Versions maintained
7. Search functional
8. Style consistent

## Non-Goals
1. Custom doc formats
2. Complex transforms
3. PDF generation
4. External hosting

## Resources
1. Haddock Documentation
2. GitHub Pages Guide
3. Documentation Best Practices