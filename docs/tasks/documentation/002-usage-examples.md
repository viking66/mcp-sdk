# Task: Create Basic Usage Examples

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: All packages
**Prerequisites**: 
- API documentation (001-api-documentation)
- All core functionality implemented

## Context
We need to create comprehensive, working examples that demonstrate the basic usage of the MCP SDK. These examples should cover common use cases, serve as learning tools, and provide a starting point for developers implementing their own MCP servers and clients.

## Requirements
1. Create server examples
2. Implement client examples
3. Document common patterns
4. Provide working code
5. Include test coverage

## Detailed Implementation Plan

### 1. Basic Server Examples

```haskell
-- File: examples/SimpleServer.hs

{-|
A basic MCP server that provides file system access.
-}
module Main where

import MCP
import MCP.Server
import MCP.Resource

main :: IO ()
main = do
    -- Initialize server
    let config = defaultConfig
            { serverName = "simple-server"
            , capabilities = [ResourceCapability]
            }
    
    server <- initServer config

    -- Add resource handler
    addResourceHandler server $ \request -> do
        case request of
            ListResources -> do
                files <- listDirectory "."
                pure $ map toResource files
            
            ReadResource path -> do
                content <- readFile path
                pure $ textResource content

    -- Run server
    runServer server
```

### 2. Basic Client Examples

```haskell
-- File: examples/SimpleClient.hs

{-|
A basic MCP client that connects to a server and reads resources.
-}
module Main where

import MCP
import MCP.Client

main :: IO ()
main = do
    -- Initialize client
    let config = defaultConfig
            { serverCommand = "simple-server"
            }
    
    client <- initClient config

    -- Connect to server
    connectClient client

    -- List available resources
    resources <- listResources client
    mapM_ print resources

    -- Read a resource
    result <- readResource client "example.txt"
    print result
```

### 3. Common Use Cases

```haskell
-- File: examples/CommonPatterns.hs

{-|
Demonstrates common patterns and best practices.
-}
module Main where

import MCP
import MCP.Server
import MCP.Resource
import MCP.Tool

-- Resource handling pattern
handleResources :: Server -> IO ()
handleResources server = do
    -- Add validation
    addResourceValidator server $ \resource -> do
        validatePath (resourcePath resource)
        validateSize (resourceSize resource)

    -- Add transformation
    addResourceTransform server $ \content -> do
        case contentType content of
            TextContent text -> 
                pure $ processText text
            BinaryContent bytes ->
                pure $ processBinary bytes

-- Tool execution pattern
handleTools :: Server -> IO ()
handleTools server = do
    -- Register tool
    let tool = Tool
            { name = "example-tool"
            , description = "An example tool"
            , schema = toolSchema
            }
    
    registerTool server tool $ \input -> do
        validateInput input
        result <- executeTool input
        processResult result
```

### 4. Directory Structure

```
examples/
├── README.md
├── basic/
│   ├── SimpleServer.hs
│   └── SimpleClient.hs
├── patterns/
│   ├── ResourcePatterns.hs
│   └── ToolPatterns.hs
├── advanced/
│   ├── CustomTransport.hs
│   └── ErrorHandling.hs
└── testing/
    ├── ServerTests.hs
    └── ClientTests.hs
```

## Example Categories

1. Basic Examples:
   - Simple server
   - Simple client
   - Resource handling
   - Tool execution

2. Common Patterns:
   - Error handling
   - Resource validation
   - Tool registration
   - Lifecycle management

3. Advanced Examples:
   - Custom transports
   - Complex workflows
   - Performance optimization
   - Security handling

## Testing Requirements

1. Example Tests:
   - All examples compile
   - All examples run
   - Error cases covered

2. Integration Tests:
   - Client-server interaction
   - Resource handling
   - Tool execution

3. Documentation Tests:
   - Comments accurate
   - Docs up to date
   - Links valid

## Files to Create/Modify
1. `examples/README.md` - Example documentation
2. `examples/basic/*` - Basic examples
3. `examples/patterns/*` - Common patterns
4. `examples/advanced/*` - Advanced examples
5. Update root `cabal.project`

## Dependencies
```yaml
dependencies:
  - mcp-core
  - mcp-server
  - mcp-client
  - filepath
  - directory
```

## Acceptance Criteria
1. All examples working
2. Documentation complete
3. Tests passing
4. Common cases covered
5. Error handling demonstrated
6. Comments clear
7. Code well-structured
8. Style consistent

## Non-Goals
1. Complex applications
2. Production code
3. Performance optimization
4. Advanced features

## Resources
1. Example Code Guidelines
2. Documentation Style Guide
3. Testing Best Practices