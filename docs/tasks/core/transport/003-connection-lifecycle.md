# Task: Implement Connection Lifecycle Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Base Transport typeclass (001-transport-typeclass)
- Stream handling types (002-stream-types)

## Context
We need to implement types and functions to manage transport connection lifecycles, handling connection establishment, maintenance, and graceful shutdown. This includes managing connection state, handling reconnection logic, and providing hooks for lifecycle events.

## Requirements
1. Define connection state types
2. Implement lifecycle events
3. Create connection management functions
4. Add reconnection handling
5. Provide lifecycle hooks

## Detailed Implementation Plan

### 1. Connection Types

```haskell
-- File: src/MCP/Core/Transport/Connection.hs

-- Connection state
data Connection = Connection
    { connectionId :: !ConnectionId
    , endpoint :: !Endpoint
    , state :: !(TVar ConnectionState)
    , stats :: !(TVar ConnectionStats)
    , events :: !EventQueue
    , config :: !ConnectionConfig
    } deriving (Eq)

newtype ConnectionId = ConnectionId { unConnectionId :: UUID }
    deriving (Eq, Show, Ord)

data ConnectionState
    = Disconnected
    | Connecting
    | Connected
    | Closing
    | Failed !ConnectionError
    deriving (Eq, Show)

-- Connection statistics
data ConnectionStats = ConnectionStats
    { connectedAt :: !UTCTime
    , lastActivity :: !UTCTime
    , bytesReceived :: !Int64
    , bytesSent :: !Int64
    , reconnectCount :: !Int
    , errorCount :: !Int
    } deriving (Eq, Show)
```

### 2. Configuration Types

```haskell
data ConnectionConfig = ConnectionConfig
    { retryPolicy :: !RetryPolicy
    , timeout :: !ConnectionTimeout
    , keepAlive :: !KeepAliveConfig
    , maxRetries :: !Int
    } deriving (Eq, Show)

data ConnectionTimeout = ConnectionTimeout
    { connectTimeout :: !NominalDiffTime
    , readTimeout :: !NominalDiffTime
    , writeTimeout :: !NominalDiffTime
    } deriving (Eq, Show)

data KeepAliveConfig = KeepAliveConfig
    { keepAliveInterval :: !NominalDiffTime
    , keepAliveTimeout :: !NominalDiffTime
    , maxMissedKeepAlives :: !Int
    } deriving (Eq, Show)
```

### 3. Event System

```haskell
data ConnectionEvent
    = StateChanged !ConnectionState !ConnectionState  -- Old, New
    | ConnectionError !ConnectionError
    | KeepAliveReceived
    | KeepAliveMissed !Int
    | StatsUpdated !ConnectionStats
    deriving (Show)

newtype EventQueue = EventQueue (TQueue ConnectionEvent)

-- Event handling
type EventHandler = ConnectionEvent -> IO ()
registerEventHandler :: Connection -> EventHandler -> IO ()
unregisterEventHandler :: Connection -> EventHandler -> IO ()
```

### 4. Lifecycle Functions

```haskell
-- Connection management
initConnection :: ConnectionConfig -> Endpoint -> IO Connection
connect :: Connection -> IO ()
disconnect :: Connection -> IO ()
reconnect :: Connection -> IO ()

-- State management
getConnectionState :: Connection -> IO ConnectionState
waitForState :: Connection -> ConnectionState -> IO ()
isConnected :: Connection -> IO Bool

-- Statistics
getConnectionStats :: Connection -> IO ConnectionStats
resetStats :: Connection -> IO ()

-- Keep-alive
sendKeepAlive :: Connection -> IO ()
handleKeepAlive :: Connection -> IO ()
```

## Testing Requirements

1. Property Tests:
   - State transitions
   - Event handling
   - Configuration validation

2. Unit Tests:
   - Lifecycle functions
   - Keep-alive handling
   - Error scenarios

3. Integration Tests:
   - Connection with streams
   - Reconnection handling
   - Event propagation

## Files to Create/Modify
1. `src/MCP/Core/Transport/Connection.hs` - Core connection types
2. `src/MCP/Core/Transport/Events.hs` - Event handling
3. `src/MCP/Core/Transport/KeepAlive.hs` - Keep-alive logic
4. `test/MCP/Core/Transport/ConnectionSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - uuid
  - time
  - async
```

## Acceptance Criteria
1. Connection lifecycle working
2. Event system functional
3. Keep-alive implemented
4. Reconnection handling works
5. Stats tracking accurate
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Implementing transport protocols
2. Authentication handling
3. Protocol-specific features
4. Complex routing logic

## Resources
1. Connection State Patterns
2. Keep-Alive Best Practices
3. Event System Design