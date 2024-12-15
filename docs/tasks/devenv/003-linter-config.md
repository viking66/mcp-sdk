# Task: Configure Code Linter

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages
**Prerequisites**: 
- HLS configuration (001-hls-config)
- Formatter setup (002-formatter-setup)

## Context
We need to set up and configure HLint for the MCP SDK project. This includes establishing linting rules, editor integration, and automated checks to maintain code quality and consistency across the project.

## Requirements
1. Configure HLint rules
2. Set up editor integration
3. Create lint checking
4. Enable automated fixes
5. Support multi-package project

## Detailed Implementation Plan

### 1. HLint Configuration

```yaml
# .hlint.yaml
- arguments: [-XTypeApplications, -XImportQualifiedPost]

# Ignore specific rules
- ignore: {name: "Use camelCase"}
- ignore: {name: "Reduce duplication"}

# Custom rules
- warn: {lhs: "return ()", rhs: "pure ()", name: "Use pure"}
- warn: {lhs: "maybe mempty", rhs: "foldMap", name: "Use foldMap"}

# Package-specific settings
- group: {name: mcp-core, paths: ["mcp-core/src/**/*.hs"]}
  rules:
    - warn: {name: "Use explicit import lists"}
    
- group: {name: mcp-server, paths: ["mcp-server/src/**/*.hs"]}
  rules:
    - warn: {name: "Use RecordWildCards"}
```

### 2. Git Hook Setup

```bash
#!/bin/sh
# .git/hooks/pre-commit

# Run hlint on staged Haskell files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=d | grep "\.hs$")
if [ -n "$STAGED_FILES" ]; then
    hlint $STAGED_FILES
    if [ $? -ne 0 ]; then
        echo "Error: Linting check failed."
        echo "Please fix the issues or use '--hlint-ignore' in your commit message to bypass."
        exit 1
    fi
fi
```

### 3. CI Check Configuration

```yaml
# .github/workflows/lint-check.yaml
name: Lint Check

on:
  pull_request:
    paths:
      - '**.hs'
      - '.hlint.yaml'

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.6.6'
          
      - name: Install HLint
        run: |
          cabal update
          cabal install hlint
          
      - name: Run Linter
        run: |
          hlint . --report=hlint-report.html
          
      - name: Upload Report
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: hlint-report
          path: hlint-report.html
```

### 4. Editor Integration

```json
// VSCode settings.json
{
  "haskell.plugin.hlint.diagnosticsOn": true,
  "haskell.plugin.hlint.codeActionsOn": true,
  "haskell.plugin.hlint.config": ".hlint.yaml"
}
```

### 5. Custom Rules

```haskell
-- File: hlint-rules.hs
module HLintRules where

import "hint" HLint.Builtin.All

warn = mcp_style
    where mcp_style = convertToExplicitImports

warn = resource_naming
    where resource_naming f = do
        when (isResourceFunction f && not (hasResourcePrefix f))
            hint "Resource functions should start with 'resource'"

warn = tool_naming
    where tool_naming f = do
        when (isToolFunction f && not (hasToolPrefix f))
            hint "Tool functions should start with 'tool'"
```

## Files to Create/Modify
1. `.hlint.yaml` - HLint configuration
2. `hlint-rules.hs` - Custom rules
3. `.git/hooks/pre-commit` - Git hook
4. `.github/workflows/lint-check.yaml` - CI check
5. Editor configuration files

## Testing Requirements

1. Lint Tests:
   - Rule application
   - Custom rules
   - Configuration loading

2. Integration Tests:
   - Editor integration
   - Git hook
   - CI workflow

3. Performance Tests:
   - Large codebase linting
   - Incremental linting
   - Rule performance

## Configuration Steps

1. Basic Setup:
   ```bash
   # Install HLint
   cabal install hlint
   
   # Create configuration
   touch .hlint.yaml
   touch hlint-rules.hs
   
   # Set up git hook
   chmod +x .git/hooks/pre-commit
   ```

2. Editor Setup:
   ```bash
   # VSCode
   code --install-extension haskell.haskell
   
   # Configure settings
   mkdir -p .vscode
   ```

3. CI Setup:
   ```bash
   # Create workflow
   mkdir -p .github/workflows
   ```

## Dependencies
```yaml
dependencies:
  - hlint >= 3.5
  - ghc-9.6.6
```

## Acceptance Criteria
1. Rules applied correctly
2. Editor integration working
3. Git hook functioning
4. CI checks passing
5. Custom rules working
6. Fast linting
7. Clear error messages
8. Easy configuration

## Non-Goals
1. Complex static analysis
2. Code generation
3. Automatic fixes
4. Style enforcement

## Resources
1. HLint Documentation
2. Custom Rule Examples
3. CI Setup Guide