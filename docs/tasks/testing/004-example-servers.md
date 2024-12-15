# Task: Implement Example Servers

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-server
**Prerequisites**: 
- All core server functionality
- Basic integration tests (003-integration-tests)

## Context
We need to implement a set of example MCP servers that demonstrate different aspects of the protocol and serve as reference implementations. These examples should cover common use cases and demonstrate best practices for server implementation.

## Requirements
1. Create example server types
2. Implement different patterns
3. Document usage examples
4. Support testing scenarios
5. Enable learning/demo use

## Detailed Implementation Plan

### 1. File System Server

```haskell
-- File: examples/FileServer/src/Main.hs

-- Core file server types
data FileServer = FileServer
    { config :: !FileServerConfig
    , root :: !FilePath
    , state :: !(TVar ServerState)
    } deriving (Eq)

data FileServerConfig = FileServerConfig
    { maxFileSize :: !Integer
    , allowedExtensions :: ![Text]
    , watchMode :: !Bool
    } deriving (Eq, Show)

-- Server implementation
initFileServer :: FileServerConfig -> IO FileServer
initFileServer config = do
    state <- newTVarIO initialState
    return FileServer{..}

-- Resource handlers
handleListFiles :: FilePath -> IO [Resource]
handleReadFile :: FilePath -> IO Content
handleWatchFile :: FilePath -> (Content -> IO ()) -> IO ()
```

### 2. Echo Server

```haskell
-- File: examples/EchoServer/src/Main.hs

-- Echo server types
data EchoServer = EchoServer
    { config :: !EchoConfig
    , history :: !(TVar [Message])
    , stats :: !(TVar EchoStats)
    } deriving (Eq)

data EchoConfig = EchoConfig
    { delay :: !NominalDiffTime
    , transform :: !(Text -> Text)
    , maxHistory :: !Int
    } deriving (Eq, Show)

-- Server implementation
handleEcho :: Message -> EchoServer -> IO Message
handleEcho msg server = do
    modifyTVar' (history server) (take maxHistory . (msg:))
    return $ transformMessage msg (config server)
```

### 3. Database Server

```haskell
-- File: examples/DbServer/src/Main.hs

-- Database server types
data DbServer = DbServer
    { config :: !DbConfig
    , pool :: !ConnectionPool
    , queries :: !QueryCache
    } deriving (Eq)

data DbConfig = DbConfig
    { connectionString :: !Text
    , poolSize :: !Int
    , maxQueries :: !Int
    } deriving (Eq, Show)

-- Server implementation
handleQuery :: Query -> DbServer -> IO QueryResult
handleExecute :: Command -> DbServer -> IO ExecuteResult
handleTransaction :: [Command] -> DbServer -> IO TransactionResult
```

### 4. WebSocket Server

```haskell
-- File: examples/WebSocketServer/src/Main.hs

-- WebSocket server types
data WsServer = WsServer
    { config :: !WsConfig
    , clients :: !(TVar (Map ClientId Client))
    , broadcast :: !(TQueue Message)
    } deriving (Eq)

data WsConfig = WsConfig
    { port :: !Int
    , pingInterval :: !NominalDiffTime
    , maxClients :: !Int
    } deriving (Eq, Show)

-- Server implementation
handleConnect :: Client -> WsServer -> IO ()
handleDisconnect :: ClientId -> WsServer -> IO ()
handleBroadcast :: Message -> WsServer -> IO ()
```

## Testing Requirements

1. Example Tests:
   - Basic functionality
   - Error handling
   - Configuration options
   - Performance characteristics

2. Integration Tests:
   - With standard clients
   - With custom clients
   - Cross-server communication
   - Error scenarios

3. Documentation Tests:
   - Usage examples
   - Configuration examples
   - Error handling examples
   - Extension examples

## Files to Create/Modify
1. `examples/FileServer/` - File system server
2. `examples/EchoServer/` - Echo server
3. `examples/DbServer/` - Database server
4. `examples/WebSocketServer/` - WebSocket server
5. Update root `cabal.project`

## Dependencies
```yaml
dependencies:
  - filepath
  - directory
  - websockets
  - persistent
  - stm
```

## Acceptance Criteria
1. All examples implemented
2. Documentation complete
3. Tests passing
4. Error handling robust
5. Examples runnable
6. Code well-commented
7. Best practices demonstrated
8. Code passes style checks

## Non-Goals
1. Production deployment
2. Complex features
3. Performance optimization
4. Advanced security

## Resources
1. MCP Server Examples Guide
2. Best Practices Documentation
3. Example Usage Patterns