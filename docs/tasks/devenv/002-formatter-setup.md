# Task: Configure Code Formatter

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages
**Prerequisites**: 
- HLS configuration (001-hls-config)

## Context
We need to set up and configure Fourmolu as our code formatter for the MCP SDK project. This includes establishing consistent formatting rules, editor integration, and automated formatting checks to maintain code style consistency across the project.

## Requirements
1. Configure Fourmolu
2. Set up editor integration
3. Create format checking
4. Enable auto-formatting
5. Support multi-package project

## Detailed Implementation Plan

### 1. Fourmolu Configuration

```yaml
# fourmolu.yaml
indentation: 2
comma-style: trailing
record-brace-space: true
newlines-between-decls: 1
haddock-style: multi-line
let-style: auto
in-style: right-align
unicode: never
respectful: true

options:
  - ImportQualifiedPost
  - BangPatterns
  - MultiWayIf
```

### 2. Git Hook Setup

```bash
#!/bin/sh
# .git/hooks/pre-commit

# Run fourmolu on staged Haskell files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=d | grep "\.hs$")
if [ -n "$STAGED_FILES" ]; then
    fourmolu --mode check $STAGED_FILES
    if [ $? -ne 0 ]; then
        echo "Error: Code formatting check failed."
        echo "Please run 'fourmolu --mode inplace' on your code."
        exit 1
    fi
fi
```

### 3. CI Check Configuration

```yaml
# .github/workflows/format-check.yaml
name: Format Check

on:
  pull_request:
    paths:
      - '**.hs'
      - 'fourmolu.yaml'

jobs:
  format:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.6.6'
          
      - name: Install Fourmolu
        run: |
          cabal update
          cabal install fourmolu
          
      - name: Check Formatting
        run: |
          fourmolu --mode check $(find . -name '*.hs')
```

### 4. Editor Integration

```json
// VSCode settings.json
{
  "[haskell]": {
    "editor.defaultFormatter": "haskell.haskell",
    "editor.formatOnSave": true
  },
  "haskell.formattingProvider": "fourmolu"
}

// Vim/Neovim configuration
// init.vim or init.lua
let g:haskell_format_on_save = 1
let g:haskell_formatter = 'fourmolu'
```

## Files to Create/Modify
1. `fourmolu.yaml` - Formatter configuration
2. `.git/hooks/pre-commit` - Git hook
3. `.github/workflows/format-check.yaml` - CI check
4. Editor configuration files
5. Update package documentation

## Testing Requirements

1. Format Tests:
   - Configuration loading
   - File formatting
   - Git hook functionality

2. Integration Tests:
   - Editor integration
   - CI workflow
   - Multi-file formatting

3. Performance Tests:
   - Large file formatting
   - Bulk formatting
   - Format checking speed

## Configuration Steps

1. Basic Setup:
   ```bash
   # Install Fourmolu
   cabal install fourmolu
   
   # Create configuration
   touch fourmolu.yaml
   
   # Set up git hook
   mkdir -p .git/hooks
   touch .git/hooks/pre-commit
   chmod +x .git/hooks/pre-commit
   ```

2. Editor Setup:
   ```bash
   # VSCode
   code --install-extension haskell.haskell
   
   # Configure settings
   mkdir -p .vscode
   touch .vscode/settings.json
   ```

3. CI Setup:
   ```bash
   # Create workflow
   mkdir -p .github/workflows
   touch .github/workflows/format-check.yaml
   ```

## Dependencies
```yaml
dependencies:
  - fourmolu >= 0.14
  - ghc-9.6.6
```

## Acceptance Criteria
1. Consistent formatting
2. Editor integration working
3. Git hook functioning
4. CI checks passing
5. Documentation updated
6. Fast formatting
7. Multi-package support
8. Easy configuration

## Non-Goals
1. Custom formatters
2. Complex formatting rules
3. Style checking
4. Code generation

## Resources
1. Fourmolu Documentation
2. Editor Integration Guide
3. CI Setup Examples