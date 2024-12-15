# Task: Implement Basic Session Management

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-server
**Prerequisites**: 
- Server initialization (001-initialization)
- Stdio transport (004-stdio-transport)
- Protocol handshake types (mcp-core/protocol/004-handshake-types)

## Context
We need to implement basic session management for MCP servers, handling client connections, session lifecycle, and cleanup. This system needs to track active sessions, manage their state, and ensure proper resource cleanup.

## Requirements
1. Define session types
2. Implement session lifecycle
3. Create session storage
4. Support session cleanup
5. Enable session tracking

## Detailed Implementation Plan

### 1. Session Types

```haskell
-- File: src/MCP/Server/Session/Types.hs

-- Core session type
data Session = Session
    { sessionId :: !SessionId
    , client :: !ClientInfo
    , transport :: !Transport
    , state :: !(TVar SessionState)
    , metrics :: !(TVar SessionMetrics)
    } deriving (Eq)

-- Client information
data ClientInfo = ClientInfo
    { clientId :: !ClientId
    , clientVersion :: !Version
    , capabilities :: ![ProtocolCapability]
    , metadata :: !Map Text Value
    } deriving (Eq, Show)

-- Session state
data SessionState
    = Initializing
    | Active
    | Idle
    | Terminating !TerminationReason
    | Terminated
    deriving (Eq, Show)

-- Session metrics
data SessionMetrics = SessionMetrics
    { created :: !UTCTime
    , lastActive :: !UTCTime
    , messagesReceived :: !Int64
    , messagesSent :: !Int64
    , errors :: !Int
    } deriving (Eq, Show)
```

### 2. Session Management

```haskell
-- Session store
data SessionStore = SessionStore
    { activeSessions :: !(TVar (Map SessionId Session))
    , sessionLimit :: !Int
    , cleanupInterval :: !NominalDiffTime
    } deriving (Eq)

-- Management functions
createSession :: ClientInfo -> Transport -> SessionStore -> IO (Either SessionError Session)
terminateSession :: SessionId -> TerminationReason -> SessionStore -> IO ()
getSession :: SessionId -> SessionStore -> IO (Maybe Session)
listSessions :: SessionStore -> IO [Session]
```

### 3. Session Lifecycle

```haskell
-- Lifecycle events
data SessionEvent
    = Connected !ClientInfo
    | Disconnected !DisconnectReason
    | StateChanged !SessionState !SessionState
    | Error !SessionError
    deriving (Eq, Show)

-- Lifecycle hooks
data SessionHooks = SessionHooks
    { onSessionCreated :: !(Session -> IO ())
    , onSessionTerminated :: !(Session -> TerminationReason -> IO ())
    , onStateChanged :: !(Session -> SessionState -> SessionState -> IO ())
    , onError :: !(Session -> SessionError -> IO ())
    }

-- Lifecycle functions
monitorSession :: Session -> SessionHooks -> IO ()
updateSessionState :: Session -> SessionState -> IO (Either SessionError ())
checkSessionHealth :: Session -> IO (Either SessionError ())
```

### 4. Session Cleanup

```haskell
-- Cleanup types
data CleanupConfig = CleanupConfig
    { maxIdleTime :: !NominalDiffTime
    , maxSessionTime :: !NominalDiffTime
    , cleanupInterval :: !NominalDiffTime
    } deriving (Eq, Show)

-- Cleanup functions
startCleanupTask :: CleanupConfig -> SessionStore -> IO ()
cleanupSession :: Session -> IO ()
cleanupResources :: Session -> IO ()
validateSessions :: SessionStore -> IO [SessionError]
```

## Testing Requirements

1. Property Tests:
   - Session lifecycle
   - State transitions
   - Cleanup behavior

2. Unit Tests:
   - Session operations
   - Store management
   - Error handling

3. Integration Tests:
   - With transport
   - Multiple sessions
   - Resource cleanup

## Files to Create/Modify
1. `src/MCP/Server/Session/Types.hs` - Core session types
2. `src/MCP/Server/Session/Store.hs` - Session storage
3. `src/MCP/Server/Session/Lifecycle.hs` - Lifecycle management
4. `test/MCP/Server/Session/SessionSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - time
  - containers
  - async
```

## Acceptance Criteria
1. Session types implemented
2. Storage working
3. Lifecycle management functional
4. Cleanup reliable
5. Error handling complete
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Session persistence
2. Complex scheduling
3. Session migration
4. Load balancing

## Resources
1. MCP Session Specification
2. Resource Management Patterns
3. Cleanup Strategy Examples