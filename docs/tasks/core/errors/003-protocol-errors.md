# Task: Implement Protocol-Specific Error Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error hierarchy (001-error-types)
- Transport errors (002-transport-errors)

## Context
We need to implement error types specific to the MCP protocol layer, covering version mismatches, capability negotiation failures, and protocol state violations. These errors are distinct from transport errors and represent protocol-level failures.

## Requirements
1. Define protocol-specific error types
2. Implement protocol state validation
3. Create capability negotiation errors
4. Add version compatibility checking
5. Support protocol handshake errors

## Detailed Implementation Plan

### 1. Protocol Error Types

```haskell
-- File: src/MCP/Core/Protocol/Error.hs

-- Protocol errors
data ProtocolError
    = VersionError !VersionError
    | CapabilityError !CapabilityError
    | StateError !StateError
    | HandshakeError !HandshakeError
    | MessageError !MessageError
    deriving (Eq, Show)

-- Version-related errors
data VersionError
    = UnsupportedVersion !Version
    | IncompatibleVersion !Version !Version  -- Client, Server
    | InvalidVersionFormat !Text
    deriving (Eq, Show)

-- Capability errors
data CapabilityError
    = UnsupportedCapability !Text
    | RequiredCapabilityMissing !Text
    | InvalidCapabilityValue !Text
    | CapabilityNegotiationFailed !Text
    deriving (Eq, Show)

-- State machine errors
data StateError
    = InvalidState !Text
    | InvalidTransition !Text !Text  -- From, To
    | UnexpectedMessage !Text
    | InvalidSequence ![Text]
    deriving (Eq, Show)

-- Handshake errors
data HandshakeError
    = HandshakeTimeout
    | InvalidInitialize !Text
    | InvalidInitialized !Text
    | HandshakeRejected !Text
    deriving (Eq, Show)

-- Message-related errors
data MessageError
    = InvalidMessageType !Text
    | InvalidMessageContent !Text
    | UnexpectedResponse !Text
    | DuplicateMessage !Text
    deriving (Eq, Show)
```

### 2. Protocol Error Context

```haskell
-- Context for protocol errors
data ProtocolErrorContext = ProtocolErrorContext
    { currentState :: !ProtocolState
    , messageHistory :: ![MessageSummary]
    , capabilities :: !Capabilities
    , timestamp :: !UTCTime
    } deriving (Eq, Show)

data ProtocolState
    = Initial
    | Initializing
    | Ready
    | ShuttingDown
    | Closed
    deriving (Eq, Show)

data MessageSummary = MessageSummary
    { messageType :: !Text
    , direction :: !Direction
    , timestamp :: !UTCTime
    } deriving (Eq, Show)

data Direction = Incoming | Outgoing
    deriving (Eq, Show)
```

### 3. Error Utilities

```haskell
-- Error construction
mkVersionError :: Version -> Version -> IO (ProtocolError, ProtocolErrorContext)
mkCapabilityError :: Text -> IO (ProtocolError, ProtocolErrorContext)
mkStateError :: Text -> Text -> IO (ProtocolError, ProtocolErrorContext)
mkHandshakeError :: Text -> IO (ProtocolError, ProtocolErrorContext)

-- Error inspection
isRecoverable :: ProtocolError -> Bool
requiresReconnect :: ProtocolError -> Bool
getErrorState :: ProtocolErrorContext -> ProtocolState

-- Error conversion
toMcpError :: (ProtocolError, ProtocolErrorContext) -> McpError
fromMcpError :: McpError -> Maybe (ProtocolError, ProtocolErrorContext)
```

## Testing Requirements

1. Property Tests:
   - State transitions
   - Version compatibility
   - Error construction

2. Unit Tests:
   - All error types
   - Error utilities
   - Context handling

3. Integration Tests:
   - Protocol state machine
   - Error handling flow
   - Recovery scenarios

## Files to Create/Modify
1. `src/MCP/Core/Protocol/Error.hs` - Error types
2. `src/MCP/Core/Protocol/State.hs` - State tracking
3. `src/MCP/Core/Protocol/Context.hs` - Context handling
4. `test/MCP/Core/Protocol/ErrorSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - time
  - text
  - containers
  - mtl
```

## Acceptance Criteria
1. All protocol error types defined
2. State machine validation working
3. Context tracking implemented
4. Version checking complete
5. Full test coverage
6. Documentation complete
7. Example error handling code
8. Code passes style checks

## Non-Goals
1. Implementing protocol logic
2. Building recovery mechanisms
3. Implementing logging system
4. Supporting deprecated protocol versions

## Resources
1. MCP Protocol Specification
2. Protocol State Machine Documentation