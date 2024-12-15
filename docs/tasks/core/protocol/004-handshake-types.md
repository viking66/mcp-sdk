# Task: Implement Protocol Handshake Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Protocol version types (001-version-types)
- Feature negotiation types (002-feature-types)
- Protocol capability types (003-capability-types)

## Context
We need to implement types for handling the initial protocol handshake between MCP clients and servers. This includes version negotiation, capability exchange, and connection establishment with proper error handling and timeout management.

## Requirements
1. Define handshake message types
2. Implement handshake state machine
3. Create timeout handling
4. Support error recovery
5. Enable handshake validation

## Detailed Implementation Plan

### 1. Handshake Messages

```haskell
-- File: src/MCP/Core/Protocol/Handshake.hs

-- Core handshake types
data HandshakeMessage
    = Initialize !InitializeRequest
    | InitializeResult !InitializeResponse
    | Initialized !InitializedNotification
    | HandshakeError !HandshakeError
    deriving (Eq, Show)

-- Initialize request
data InitializeRequest = InitializeRequest
    { clientId :: !ClientId
    , clientVersion :: !Version
    , clientCapabilities :: ![ProtocolCapability]
    , initOptions :: !InitOptions
    } deriving (Eq, Show)

-- Initialize response
data InitializeResponse = InitializeResponse
    { serverId :: !ServerId
    , serverVersion :: !Version
    , serverCapabilities :: ![ProtocolCapability]
    , negotiatedFeatures :: !NegotiatedFeatures
    } deriving (Eq, Show)

-- Initialized notification
data InitializedNotification = InitializedNotification
    { timestamp :: !UTCTime
    , sessionId :: !SessionId
    } deriving (Eq, Show)
```

### 2. Handshake State Machine

```haskell
-- State types
data HandshakeState
    = NotStarted
    | Initializing !InitializeRequest
    | AwaitingResponse
    | Responding !InitializeResponse
    | AwaitingInitialized
    | Complete !HandshakeResult
    | Failed !HandshakeError
    deriving (Eq, Show)

-- State machine
data HandshakeMachine = HandshakeMachine
    { currentState :: !(TVar HandshakeState)
    , config :: !HandshakeConfig
    , timeouts :: !TimeoutConfig
    } deriving (Eq)

-- State transitions
data HandshakeTransition
    = StartHandshake !InitializeRequest
    | ReceiveResponse !InitializeResponse
    | ReceiveInitialized !InitializedNotification
    | HandleTimeout
    | HandleError !HandshakeError
    deriving (Eq, Show)
```

### 3. Timeout Management

```haskell
-- Timeout configuration
data TimeoutConfig = TimeoutConfig
    { initializeTimeout :: !NominalDiffTime
    , responseTimeout :: !NominalDiffTime
    , initializedTimeout :: !NominalDiffTime
    } deriving (Eq, Show)

-- Timeout handling
data TimeoutState = TimeoutState
    { startTime :: !UTCTime
    , deadlines :: ![(HandshakePhase, UTCTime)]
    } deriving (Eq, Show)

data HandshakePhase
    = InitializePhase
    | ResponsePhase
    | InitializedPhase
    deriving (Eq, Show)

-- Timeout functions
checkTimeout :: TimeoutState -> HandshakePhase -> IO Bool
updateDeadlines :: HandshakePhase -> TimeoutState -> IO TimeoutState
```

### 4. Error Handling

```haskell
-- Error types
data HandshakeError
    = VersionMismatch !Version !Version
    | UnsupportedCapability !Text
    | InvalidMessage !Text
    | TimeoutError !HandshakePhase
    | StateError !HandshakeState !HandshakeTransition
    deriving (Eq, Show)

-- Recovery handling
data RecoveryAction
    = RetryHandshake
    | AbortHandshake
    | DowngradeVersion !Version
    | ReduceCapabilities ![ProtocolCapability]
    deriving (Eq, Show)

-- Recovery functions
determineRecovery :: HandshakeError -> Maybe RecoveryAction
attemptRecovery :: RecoveryAction -> HandshakeMachine -> IO (Either HandshakeError HandshakeState)
```

## Testing Requirements

1. Property Tests:
   - State transitions
   - Timeout handling
   - Error recovery

2. Unit Tests:
   - All message types
   - State machine
   - Recovery actions

3. Integration Tests:
   - Complete handshakes
   - Error scenarios
   - Timeout behavior

## Files to Create/Modify
1. `src/MCP/Core/Protocol/Handshake.hs` - Core handshake types
2. `src/MCP/Core/Protocol/State.hs` - State machine
3. `src/MCP/Core/Protocol/Timeout.hs` - Timeout handling
4. `test/MCP/Core/Protocol/HandshakeSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - time
  - uuid
  - async
```

## Acceptance Criteria
1. Handshake types implemented
2. State machine working
3. Timeout handling functional
4. Error recovery complete
5. Recovery actions working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Authentication
2. Complex recovery
3. Session management
4. Connection pooling

## Resources
1. MCP Protocol Specification
2. State Machine Patterns
3. Error Recovery Examples