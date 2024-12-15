# Task: Implement Client SSE Transport

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: mcp-client
**Prerequisites**: 
- Connection management (005-connection-management)
- Server SSE transport (mcp-server/advanced/004-sse-transport)

## Context
We need to implement the client-side Server-Sent Events (SSE) transport for MCP. This transport needs to handle HTTP-based event streaming from servers while managing POST requests for client-to-server messages. The implementation should handle connection management, event processing, and error recovery.

## Requirements
1. Define SSE client types
2. Implement event handling
3. Create HTTP management
4. Support connection handling
5. Enable error recovery

## Detailed Implementation Plan

### 1. Transport Types

```haskell
-- File: src/MCP/Client/Transport/SSE.hs

-- Core SSE types
data SSETransport = SSETransport
    { config :: !SSEConfig
    , eventSource :: !EventSource
    , httpClient :: !HTTPClient
    , state :: !(TVar TransportState)
    } deriving (Eq)

data SSEConfig = SSEConfig
    { url :: !Text
    , headers :: ![(Text, Text)]
    , options :: !SSEOptions
    , retryPolicy :: !RetryPolicy
    } deriving (Eq, Show)

data SSEOptions = SSEOptions
    { reconnectTime :: !Int
    , withCredentials :: !Bool
    , timeout :: !NominalDiffTime
    } deriving (Eq, Show)
```

### 2. Event Handling

```haskell
-- Event types
data EventSource = EventSource
    { sourceId :: !SourceId
    , connection :: !Connection
    , handlers :: !EventHandlers
    , buffer :: !(TQueue Event)
    } deriving (Eq)

data Event = Event
    { eventId :: !(Maybe Text)
    , eventType :: !(Maybe Text)
    , data_ :: !Text
    , retry :: !(Maybe Int)
    } deriving (Eq, Show)

-- Event handling functions
handleEvent :: Event -> EventSource -> IO ()
processEvent :: Event -> SSETransport -> IO (Either EventError ())
validateEvent :: Event -> Either ValidationError ()
```

### 3. HTTP Management

```haskell
-- HTTP types
data HTTPClient = HTTPClient
    { manager :: !Manager
    , settings :: !ManagerSettings
    , metrics :: !(TVar HTTPMetrics)
    } deriving (Eq)

data HTTPRequest = HTTPRequest
    { method :: !Method
    , path :: !Text
    , body :: !(Maybe ByteString)
    , headers :: ![(Text, Text)]
    } deriving (Eq, Show)

-- HTTP functions
sendRequest :: HTTPRequest -> HTTPClient -> IO (Either HTTPError Response)
createRequest :: Method -> Text -> HTTPRequest
handleResponse :: Response -> Either HTTPError ByteString
```

### 4. Connection Management

```haskell
-- Connection types
data Connection = Connection
    { stream :: !EventStream
    , lastEventId :: !(TVar (Maybe Text))
    , reconnectDelay :: !(TVar Int)
    , state :: !(TVar ConnectionState)
    } deriving (Eq)

data ConnectionState
    = Connecting
    | Connected
    | Reconnecting !Int
    | Disconnected !DisconnectReason
    deriving (Eq, Show)

-- Connection functions
connect :: SSEConfig -> IO (Either ConnectionError EventSource)
disconnect :: EventSource -> IO ()
reconnect :: EventSource -> IO (Either ConnectionError ())
```

## Testing Requirements

1. Unit Tests:
   - Event parsing
   - HTTP handling
   - Connection lifecycle
   - Error scenarios

2. Integration Tests:
   - With SSE server
   - Reconnection handling
   - Event processing

3. Property Tests:
   - Event properties
   - Connection states
   - Error handling

## Files to Create/Modify
1. `src/MCP/Client/Transport/SSE.hs` - Core SSE transport
2. `src/MCP/Client/Transport/HTTP.hs` - HTTP client
3. `src/MCP/Client/Transport/Event.hs` - Event handling
4. `test/MCP/Client/Transport/SSESpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - http-client
  - http-types
  - stm
  - async
```

## Acceptance Criteria
1. SSE transport working
2. Event handling functional
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
2. HTTP Client Best Practices
3. Event Stream Patterns