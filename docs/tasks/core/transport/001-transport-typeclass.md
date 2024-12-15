# Task: Implement Base Transport Typeclass

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error types (001-error-types)
- Transport errors (002-transport-errors)

## Context
We need to define the foundational Transport typeclass that all MCP transport implementations will implement. This typeclass needs to provide a clean abstraction over different transport mechanisms (stdio, SSE, etc.) while maintaining type safety and proper error handling.

## Requirements
1. Define the Transport typeclass
2. Create transport configuration types
3. Define connection lifecycle methods
4. Add message sending/receiving capabilities
5. Support backpressure handling

## Detailed Implementation Plan

### 1. Transport Types

```haskell
-- File: src/MCP/Core/Transport/Types.hs

-- Transport configuration
data TransportConfig = TransportConfig
    { bufferSize :: !Int
    , timeout :: !NominalDiffTime
    , retryPolicy :: !RetryPolicy
    } deriving (Show, Eq)

-- Transport statistics
data TransportStats = TransportStats
    { bytesReceived :: !Int64
    , bytesSent :: !Int64
    , messagesReceived :: !Int64
    , messagesSent :: !Int64
    , connectedAt :: !UTCTime
    } deriving (Show, Eq)

-- Message types
newtype Message = Message
    { rawBytes :: ByteString
    } deriving (Show, Eq)

-- Transport status
data TransportStatus
    = Connected
    | Disconnected
    | Failed !TransportError
    deriving (Show, Eq)

-- Retry policy
data RetryPolicy = RetryPolicy
    { maxAttempts :: !Int
    , baseDelay :: !NominalDiffTime
    , maxDelay :: !NominalDiffTime
    } deriving (Show, Eq)
```

### 2. Transport Typeclass

```haskell
-- The core Transport typeclass
class Transport t where
    -- Initialize transport
    initialize :: TransportConfig -> t -> IO ()
    
    -- Connection management
    connect :: t -> IO ()
    disconnect :: t -> IO ()
    
    -- Message handling
    send :: Message -> t -> IO ()
    receive :: t -> IO Message
    
    -- Status and monitoring
    getStatus :: t -> IO TransportStatus
    getStats :: t -> IO TransportStats
    
    -- Optional methods with default implementations
    flush :: t -> IO ()
    flush = pure ()
    
    reset :: t -> IO ()
    reset = pure ()

-- Transport creation
createTransport :: TransportConfig -> IO t
```

### 3. Utility Functions

```haskell
-- Transport utilities
withTransport :: Transport t => t -> (t -> IO a) -> IO a
reconnect :: Transport t => t -> IO ()
tryReconnect :: Transport t => RetryPolicy -> t -> IO ()

-- Message utilities
packMessage :: ByteString -> Message
unpackMessage :: Message -> ByteString
validateMessage :: Message -> Either TransportError ()

-- Stats utilities
updateStats :: TransportStats -> Message -> TransportStats
resetStats :: UTCTime -> TransportStats
```

### 4. Default Implementations

```haskell
-- Base transport implementation
data BaseTransport = BaseTransport
    { config :: !TransportConfig
    , stats :: !TVar TransportStats
    , status :: !TVar TransportStatus
    }

-- Default implementation helpers
defaultInitialize :: BaseTransport -> IO ()
defaultConnect :: BaseTransport -> IO ()
defaultDisconnect :: BaseTransport -> IO ()
```

## Testing Requirements

1. Property Tests:
   - Message handling
   - Stats tracking
   - Status transitions

2. Unit Tests:
   - All typeclass methods
   - Utility functions
   - Default implementations

3. Integration Tests:
   - Connection lifecycle
   - Error handling
   - Retry behavior

## Files to Create/Modify
1. `src/MCP/Core/Transport/Types.hs` - Core types
2. `src/MCP/Core/Transport/Class.hs` - Typeclass definition
3. `src/MCP/Core/Transport/Base.hs` - Base implementation
4. `test/MCP/Core/Transport/TypesSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - bytestring
  - stm
  - time
  - async
```

## Acceptance Criteria
1. Transport typeclass defined
2. Base implementation working
3. Message handling functional
4. Stats tracking implemented
5. Full test coverage
6. Documentation complete
7. Example implementations
8. Code passes style checks

## Non-Goals
1. Implementing specific transports
2. Building protocol layer
3. Implementing encryption
4. Supporting streaming

## Resources
1. Transport Protocol Specification
2. Haskell Typeclass Best Practices