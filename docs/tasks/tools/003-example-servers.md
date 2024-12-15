# Task: Implement Example Servers

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: mcp-server
**Prerequisites**: All core functionality implemented

## Context
We need to create a set of example MCP servers that demonstrate different use cases and implementation patterns. These examples should be well-documented, follow best practices, and serve as learning resources for developers.

## Requirements
1. Create file server example
2. Implement database server
3. Add tool server example
4. Support resource examples
5. Enable prompt examples

## Detailed Implementation Plan

### 1. File System Server

```haskell
-- File: examples/file-server/src/Main.hs

module Main where

import MCP.Core
import MCP.Server
import System.Directory
import System.FilePath

data FileServer = FileServer
    { root :: !FilePath
    , config :: !ServerConfig
    , state :: !(TVar ServerState)
    }

-- Server initialization
initFileServer :: FilePath -> IO FileServer
initFileServer root = do
    config <- defaultConfig
    state <- newTVarIO initialState
    pure FileServer{..}

-- Resource handlers
handleListFiles :: FilePath -> IO [Resource]
handleListFiles path = do
    entries <- listDirectory path
    traverse toResource entries
  where
    toResource entry = do
        isDir <- doesDirectoryExist entry
        pure $ Resource
            { uri = fileToUri entry
            , name = Text.pack entry
            , isDirectory = isDir
            }

handleReadFile :: FilePath -> IO Content
handleReadFile path = do
    content <- readFile path
    pure $ TextContent content
```

### 2. Database Server

```haskell
-- File: examples/db-server/src/Main.hs

module Main where

import MCP.Core
import MCP.Server
import Database.PostgreSQL.Simple

data DbServer = DbServer
    { conn :: !Connection
    , config :: !ServerConfig
    , state :: !(TVar ServerState)
    }

-- Server initialization
initDbServer :: ConnectInfo -> IO DbServer
initDbServer info = do
    conn <- connect info
    config <- defaultConfig
    state <- newTVarIO initialState
    pure DbServer{..}

-- Tool handlers
handleQuery :: Query -> IO [Row]
handleQuery query = do
    result <- execute conn query
    pure $ map toRow result

handleExecute :: Command -> IO Int64
handleExecute cmd = do
    execute conn cmd
```

### 3. Tool Server

```haskell
-- File: examples/tool-server/src/Main.hs

module Main where

import MCP.Core
import MCP.Server
import System.Process

data ToolServer = ToolServer
    { tools :: ![Tool]
    , config :: !ServerConfig
    , state :: !(TVar ServerState)
    }

-- Tool definitions
gitTool :: Tool
gitTool = Tool
    { name = "git"
    , description = "Git operations"
    , schema = gitSchema
    }

-- Tool handlers
handleGitCommand :: Value -> IO ToolResult
handleGitCommand input = do
    cmd <- parseGitCommand input
    result <- readProcess "git" (words cmd) ""
    pure $ TextResult result

shellTool :: Tool
shellTool = Tool
    { name = "shell"
    , description = "Shell command execution"
    , schema = shellSchema
    }
```

### 4. Resource Server

```haskell
-- File: examples/resource-server/src/Main.hs

module Main where

import MCP.Core
import MCP.Server
import qualified Data.Map as Map

data ResourceServer = ResourceServer
    { resources :: !(TVar (Map ResourceId Resource))
    , config :: !ServerConfig
    , state :: !(TVar ServerState)
    }

-- Resource management
addResource :: Resource -> ResourceServer -> IO ()
addResource resource server = atomically $ do
    modifyTVar' (resources server) $ Map.insert (resourceId resource) resource

updateResource :: ResourceId -> Resource -> ResourceServer -> IO ()
updateResource rid resource server = atomically $ do
    modifyTVar' (resources server) $ Map.insert rid resource

-- Subscription handling
handleSubscribe :: ResourcePattern -> Subscriber -> IO Subscription
handleSubscribe pattern subscriber = do
    sub <- createSubscription pattern subscriber
    registerSubscription sub
    pure sub
```

## Testing Requirements

1. Example Tests:
   - All examples working
   - Error handling
   - Configuration

2. Integration Tests:
   - With clients
   - Multiple servers
   - Resource handling

3. Documentation Tests:
   - README files
   - Code comments
   - Usage examples

## Files to Create/Modify
1. Create all example servers
2. Add documentation
3. Create test suites
4. Add deployment examples
5. Create Docker configurations

## Dependencies
```yaml
dependencies:
  - mcp-core
  - mcp-server
  - postgresql-simple
  - process
```

## Acceptance Criteria
1. All examples working
2. Documentation clear
3. Tests passing
4. Error handling robust
5. Configuration flexible
6. Easy to understand
7. Best practices followed
8. Code passes checks

## Non-Goals
1. Production ready
2. Complex features
3. Performance optimization
4. Advanced security

## Resources
1. MCP Server Examples
2. Implementation Patterns
3. Best Practices Guide