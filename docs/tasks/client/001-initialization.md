# Task: Implement Client Initialization

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-client
**Prerequisites**: 
- Core protocol types (mcp-core)
- Protocol handshake types (mcp-core/protocol/004-handshake-types)

## Context
We need to implement the client initialization process for MCP, handling client configuration, startup, transport connection, and handshake with servers. The implementation needs to be robust and handle various error conditions while providing a clean API.

## Requirements
1. Define client configuration types
2. Implement client lifecycle management
3. Create transport connection
4. Support startup validation
5. Enable shutdown handling

## Detailed Implementation Plan

### 1. Client Configuration

```haskell
-- File: src/MCP/Client/Config.hs

-- Core client config
data ClientConfig = ClientConfig
    { clientId :: !ClientId
    , version :: !Version
    , capabilities :: ![ProtocolCapability]
    , transports :: ![TransportConfig]
    , options :: !ClientOptions
    } deriving (Eq, Show)

-- Client options
data ClientOptions = ClientOptions
    { connectTimeout :: !NominalDiffTime
    , retryPolicy :: !RetryPolicy
    , keepAlive :: !Bool
    , logLevel :: !LogLevel
    } deriving (Eq, Show)

-- Transport configuration
data TransportConfig
    = StdioConfig !StdioOptions
    | SseConfig !SseOptions
    | CustomConfig !Text !Value
    deriving (Eq, Show)
```

### 2. Client State

```haskell
-- Client state management
data ClientState
    = Uninitialized
    | Connecting !ConnectPhase
    | Connected !ConnectedState
    | Disconnecting !DisconnectReason
    | Failed !ClientError
    deriving (Eq, Show)

data ConnectPhase
    = TransportConnect
    | ProtocolHandshake
    | CapabilityNegotiation
    | Initializing
    deriving (Eq, Show)

data ConnectedState = ConnectedState
    { serverInfo :: !ServerInfo
    , transport :: !Transport
    , capabilities :: ![NegotiatedCapability]
    , metrics :: !ClientMetrics
    } deriving (Eq, Show)
```

### 3. Initialization Process

```haskell
-- Core initialization types
data ClientInit = ClientInit
    { config :: !ClientConfig
    , environment :: !Environment
    , hooks :: !ClientHooks
    } deriving (Eq)

-- Environment setup
data Environment = Environment
    { variables :: !(Map Text Text)
    , workingDirectory :: !FilePath
    , tempDirectory :: !FilePath
    } deriving (Eq, Show)

-- Lifecycle hooks
data ClientHooks = ClientHooks
    { onConnecting :: !(ConnectPhase -> IO ())
    , onConnected :: !(ConnectedState -> IO ())
    , onDisconnecting :: !(DisconnectReason -> IO ())
    , onError :: !(ClientError -> IO ())
    }

-- Initialization functions
initializeClient :: ClientInit -> IO (Either ClientError Client)
validateConfig :: ClientConfig -> Either ConfigError ()
setupTransport :: TransportConfig -> IO (Either ClientError Transport)
```

### 4. Shutdown Handling

```haskell
-- Shutdown types
data ShutdownRequest = ShutdownRequest
    { reason :: !ShutdownReason
    , timeout :: !NominalDiffTime
    , force :: !Bool
    } deriving (Eq, Show)

data ShutdownReason
    = UserRequested
    | ServerDisconnected
    | TransportError !TransportError
    | ProtocolError !ProtocolError
    deriving (Eq, Show)

-- Shutdown functions
initiateShutdown :: ShutdownRequest -> Client -> IO ()
waitForShutdown :: Client -> IO (Either ClientError ())
cleanupResources :: Client -> IO ()
```

## Testing Requirements

1. Unit Tests:
   - Config validation
   - State transitions
   - Error handling

2. Integration Tests:
   - Server connection
   - Transport setup
   - Shutdown process

3. Property Tests:
   - Configuration generation
   - State machine properties
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Client/Config.hs` - Client configuration
2. `src/MCP/Client/Init.hs` - Initialization logic
3. `src/MCP/Client/State.hs` - State management
4. `test/MCP/Client/InitSpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - async
  - retry
  - filepath
```

## Acceptance Criteria
1. Client config implemented
2. Initialization working
3. Transport setup functional
4. State management complete
5. Shutdown handling working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex connection pooling
2. Authentication handling
3. Advanced retry strategies
4. Connection tunneling

## Resources
1. MCP Client Specification
2. Client Lifecycle Patterns
3. Error Handling Examples