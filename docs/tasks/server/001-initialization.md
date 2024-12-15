# Task: Implement Server Initialization

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-server
**Prerequisites**: 
- Core protocol types (mcp-core)
- Protocol handshake types (mcp-core/protocol/004-handshake-types)

## Context
We need to implement the server initialization process for MCP, handling server configuration, startup, transport setup, and initial handshake with clients. This system needs to be robust, properly handle errors, and support different transport types.

## Requirements
1. Define server configuration types
2. Implement server lifecycle management
3. Create transport initialization
4. Support startup validation
5. Enable shutdown handling

## Detailed Implementation Plan

### 1. Server Configuration

```haskell
-- File: src/MCP/Server/Config.hs

-- Core server config
data ServerConfig = ServerConfig
    { serverId :: !ServerId
    , version :: !Version
    , capabilities :: ![ProtocolCapability]
    , transports :: ![TransportConfig]
    , options :: !ServerOptions
    } deriving (Eq, Show)

-- Server options
data ServerOptions = ServerOptions
    { maxConnections :: !Int
    , handshakeTimeout :: !NominalDiffTime
    , keepAliveInterval :: !NominalDiffTime
    , logLevel :: !LogLevel
    } deriving (Eq, Show)

-- Transport configuration
data TransportConfig
    = StdioConfig !StdioOptions
    | SseConfig !SseOptions
    | CustomConfig !Text !Value
    deriving (Eq, Show)
```

### 2. Server State

```haskell
-- Server state management
data ServerState
    = Starting
    | Running !RunningState
    | ShuttingDown !ShutdownReason
    | Failed !ServerError
    deriving (Eq, Show)

data RunningState = RunningState
    { activeConnections :: !(TVar (Map ConnectionId Connection))
    , transportStates :: !(TVar (Map TransportId TransportState))
    , statistics :: !(TVar ServerStats)
    } deriving (Eq)

data ServerStats = ServerStats
    { startTime :: !UTCTime
    , totalConnections :: !Int64
    , activeRequests :: !Int
    , errorCount :: !Int64
    } deriving (Eq, Show)
```

### 3. Initialization Process

```haskell
-- Server initialization
data ServerInit = ServerInit
    { config :: !ServerConfig
    , environment :: !Environment
    , hooks :: !ServerHooks
    } deriving (Eq)

-- Environment
data Environment = Environment
    { variables :: !(Map Text Text)
    , workingDirectory :: !FilePath
    , tempDirectory :: !FilePath
    } deriving (Eq, Show)

-- Lifecycle hooks
data ServerHooks = ServerHooks
    { onStarting :: !(IO ())
    , onRunning :: !(IO ())
    , onShutdown :: !(IO ())
    , onError :: !(ServerError -> IO ())
    }

-- Initialization functions
initializeServer :: ServerInit -> IO (Either ServerError Server)
validateConfig :: ServerConfig -> Either ConfigError ()
setupTransports :: [TransportConfig] -> IO (Either ServerError [Transport])
```

### 4. Shutdown Handling

```haskell
-- Shutdown types
data ShutdownReason
    = GracefulShutdown
    | ErrorShutdown !ServerError
    | TimeoutShutdown
    deriving (Eq, Show)

-- Shutdown process
data ShutdownProcess = ShutdownProcess
    { timeout :: !NominalDiffTime
    , force :: !Bool
    , reason :: !ShutdownReason
    } deriving (Eq, Show)

-- Shutdown functions
initiateShutdown :: ShutdownProcess -> Server -> IO ()
waitForShutdown :: Server -> IO (Either ServerError ())
cleanupResources :: Server -> IO ()
```

## Testing Requirements

1. Property Tests:
   - Configuration validation
   - State transitions
   - Initialization process

2. Unit Tests:
   - All server components
   - Transport setup
   - Shutdown process

3. Integration Tests:
   - Complete server lifecycle
   - Multiple transports
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Server/Config.hs` - Server configuration
2. `src/MCP/Server/Init.hs` - Initialization logic
3. `src/MCP/Server/State.hs` - State management
4. `test/MCP/Server/InitSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - async
  - time
  - filepath
```

## Acceptance Criteria
1. Server config implemented
2. Initialization working
3. Transport setup functional
4. State management complete
5. Shutdown handling working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex clustering
2. Hot reloading
3. Dynamic configuration
4. State persistence

## Resources
1. MCP Server Specification
2. Server Lifecycle Patterns
3. Resource Management Examples