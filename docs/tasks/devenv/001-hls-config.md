# Task: Configure Haskell Language Server

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: All packages

## Context
We need to configure Haskell Language Server (HLS) for the MCP SDK project to provide a consistent and productive development environment. This includes setting up proper compiler options, plugins, and editor configurations to support efficient development.

## Requirements
1. Configure HLS settings
2. Set up plugins
3. Configure IDE support
4. Enable code actions
5. Support multi-package project

## Detailed Implementation Plan

### 1. Project Configuration

```yaml
# hie.yaml
cradle:
  cabal:
    - path: "./mcp-core/src"
      component: "lib:mcp-core"
    - path: "./mcp-core/test"
      component: "test:mcp-core-test"
    - path: "./mcp-server/src"
      component: "lib:mcp-server"
    - path: "./mcp-server/test"
      component: "test:mcp-server-test"
    - path: "./mcp-client/src"
      component: "lib:mcp-client"
    - path: "./mcp-client/test"
      component: "test:mcp-client-test"
```

### 2. Plugin Configuration

```yaml
# .hls.yaml
formatOnImportOn: true
checkProject: true

plugins:
  - name: ghcide
    enabled: true
    options:
      maxCompletions: 50
      snippetsOn: true

  - name: hls-hlint-plugin
    enabled: true
    config:
      flags: []

  - name: hls-class-plugin
    enabled: true

  - name: hls-retrie-plugin
    enabled: true

  - name: hls-explicit-imports-plugin
    enabled: true

  - name: hls-module-name-plugin
    enabled: true
```

### 3. Editor Configuration

```json
// VSCode settings.json
{
  "haskell.formattingProvider": "fourmolu",
  "haskell.checkProject": true,
  "haskell.plugin.importLens.codeActionsOn": true,
  "haskell.plugin.importLens.codeLensOn": true,
  "haskell.trace.server": "messages",
  "haskell.serverEnvironment": {
    "PATH": "${PATH}:${workspaceRoot}/.nix/bin"
  }
}
```

### 4. GHC Options

```cabal
-- Common GHC options for all packages
common warnings
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wmissing-home-modules
    -Wpartial-fields
    -Wredundant-constraints
```

## Files to Create/Modify
1. `hie.yaml` - HLS project configuration
2. `.hls.yaml` - HLS settings
3. `.vscode/settings.json` - VSCode settings
4. Update all .cabal files with GHC options
5. Create plugin configuration files

## Testing Requirements

1. Environment Tests:
   - HLS starts correctly
   - Project loads properly
   - Plugins function

2. Feature Tests:
   - Code completion
   - Jump to definition
   - Find references
   - Code actions

3. Performance Tests:
   - Project load time
   - Code completion speed
   - Analysis time

## Configuration Steps

1. Basic Setup:
   ```bash
   # Install HLS
   ghcup install hls 2.6.0
   
   # Create configuration
   touch hie.yaml
   touch .hls.yaml
   
   # Configure VSCode
   mkdir -p .vscode
   touch .vscode/settings.json
   ```

2. Plugin Setup:
   ```bash
   # Install required plugins
   cabal install hlint
   cabal install fourmolu
   cabal install retrie
   ```

3. Editor Setup:
   - Install Haskell extension
   - Configure settings
   - Set up keybindings

## Dependencies
```yaml
dependencies:
  - haskell-language-server-9.6.6
  - ghc-9.6.6
  - hlint
  - fourmolu
```

## Acceptance Criteria
1. HLS starts cleanly
2. All plugins working
3. Fast project loading
4. Code navigation works
5. Completions working
6. Code actions available
7. Documentation accessible
8. Multi-package support working

## Non-Goals
1. Custom plugins
2. Complex refactoring
3. Style automation
4. Build integration

## Resources
1. HLS Configuration Guide
2. VSCode Haskell Guide
3. Plugin Documentation