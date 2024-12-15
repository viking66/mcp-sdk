# Task: Implement SSE Transport

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: mcp-server
**Prerequisites**: 
- Basic transport implementation (004-stdio-transport)
- Protocol capability types (mcp-core/protocol/003-capability-types)

## Context
We need to implement Server-Sent Events (SSE) transport for MCP. This transport will enable HTTP-based server-to-client streaming while using HTTP POST for client-to-server messages. The implementation needs to handle connection management, event streaming, and proper error handling.

## Requirements
1. Define SSE transport types
2. Implement event streaming
3. Create HTTP handling
4. Support connection management
5. Enable error handling

## Detailed Implementation Plan

### 1. Transport Types

```haskell
-- File: src/MCP/Server/Transport/SSE.hs

-- Core SSE types
data SSETransport = SSETransport
    { config :: !SSEConfig
    , server :: !WAIServer
    , connections :: !(TVar (Map ConnectionId Connection))
    , state :: !(TVar TransportState)
    } deriving (Eq)

data SSEConfig = SSEConfig
    { port :: !Int
    , host :: !Text
    , path :: !Text
    , options :: !SSEOptions
    } deriving (Eq, Show)

data SSEOptions = SSEOptions
    { keepAlive :: !Bool
    , reconnectTime :: !Int
    , compression :: !Bool
    } deriving (Eq, Show)
```

### 2. Event Streaming

```haskell
-- Event types
data SSEEvent = SSEEvent
    { eventId :: !(Maybe Text)
    , eventType :: !(Maybe Text)
    , data_ :: !Text
    , retry :: !(Maybe Int)
    } deriving (Eq, Show)

data EventStream = EventStream
    { source :: !EventSource
    , sink :: !EventSink
    , config :: !StreamConfig
    } deriving (Eq)

-- Streaming functions
sendEvent :: SSEEvent -> Connection -> IO (Either TransportError ())
broadcastEvent :: SSEEvent -> SSETransport -> IO ()
handleEventStream :: EventStream -> Connection -> IO ()
```

### 3. HTTP Handling

```haskell
-- HTTP types
data HTTPHandler = HTTPHandler
    { transport :: !SSETransport
    , router :: !HTTPRouter
    , middleware :: ![Middleware]
    } deriving (Eq)

data HTTPRouter = HTTPRouter
    { routes :: ![Route]
    , notFound :: !Application
    , errorHandler :: !ErrorHandler
    } deriving (Eq)

-- HTTP functions
handleSSE :: Connection -> HTTPHandler -> Application
handlePost :: Connection -> HTTPHandler -> Application
setupMiddleware :: [Middleware] -> Application -> Application
```

### 4. Connection Management

```haskell
-- Connection types
data Connection = Connection
    { connectionId :: !ConnectionId
    , request :: !Request
    , response :: !Response
    , stream :: !EventStream
    , state :: !(TVar ConnectionState)
    } deriving (Eq)

data ConnectionState
    = Connecting
    | Connected
    | Disconnecting !DisconnectReason
    | Failed !TransportError
    deriving (Eq, Show)

-- Connection functions
acceptConnection :: Request -> SSETransport -> IO (Either TransportError Connection)
closeConnection :: DisconnectReason -> Connection -> IO ()
monitorConnection :: Connection -> IO ()
```

## Testing Requirements

1. Unit Tests:
   - Event formatting
   - HTTP handling
   - Connection lifecycle
   - Error scenarios

2. Integration Tests:
   - With real clients
   - Connection handling
   - Event delivery

3. Property Tests:
   - Event properties
   - Connection states
   - Error handling

## Files to Create/Modify
1. `src/MCP/Server/Transport/SSE.hs` - Core SSE transport
2. `src/MCP/Server/Transport/HTTP.hs` - HTTP handling
3. `src/MCP/Server/Transport/Events.hs` - Event system
4. `test/MCP/Server/Transport/SSESpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - wai
  - warp
  - http-types
  - stm
```

## Acceptance Criteria
1. SSE transport working
2. Event streaming functional
3. HTTP handling complete
4. Connection management working
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. WebSocket support
2. Complex routing
3. Authentication
4. Load balancing

## Resources
1. SSE Specification
2. WAI Documentation
3. HTTP/2 Best Practices