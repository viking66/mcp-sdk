# Task: Implement Debugging Utilities

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: mcp-core
**Prerequisites**: All core functionality implemented

## Context
We need to implement debugging utilities to help developers troubleshoot MCP implementations. These utilities should provide insight into message flows, state transitions, and error conditions while being easy to use and configure.

## Requirements
1. Create message debugger
2. Implement state inspector
3. Add logging utilities
4. Support trace collection
5. Enable traffic analysis

## Detailed Implementation Plan

### 1. Message Debugger

```haskell
-- File: src/MCP/Debug/Message.hs

-- Message debugger types
data MessageDebugger = MessageDebugger
    { config :: !DebuggerConfig
    , filters :: ![MessageFilter]
    , handlers :: ![MessageHandler]
    , state :: !(TVar DebuggerState)
    } deriving (Eq)

data DebuggerConfig = DebuggerConfig
    { captureLimit :: !Int
    , detailLevel :: !DetailLevel
    , outputFormat :: !OutputFormat
    } deriving (Eq, Show)

-- Debugging functions
debugMessage :: Message -> MessageDebugger -> IO ()
inspectMessage :: Message -> MessageDebugger -> IO MessageAnalysis
compareMessages :: Message -> Message -> MessageComparison
```

### 2. State Inspector

```haskell
-- File: src/MCP/Debug/State.hs

-- State inspection types
data StateInspector = StateInspector
    { snapshots :: !(TVar [StateSnapshot])
    , diffing :: !DiffConfig
    , triggers :: ![StateTrigger]
    } deriving (Eq)

data StateSnapshot = StateSnapshot
    { timestamp :: !UTCTime
    , state :: !AppState
    , context :: !Context
    } deriving (Eq, Show)

-- Inspection functions
captureState :: AppState -> StateInspector -> IO StateSnapshot
compareStates :: StateSnapshot -> StateSnapshot -> StateDiff
watchStateChanges :: StateInspector -> (StateDiff -> IO ()) -> IO ()
```

### 3. Logging Utilities

```haskell
-- File: src/MCP/Debug/Log.hs

-- Logging types
data DebugLogger = DebugLogger
    { level :: !LogLevel
    , outputs :: ![LogOutput]
    , format :: !LogFormat
    , context :: !LogContext
    } deriving (Eq)

data LogOutput
    = ConsoleOutput !ConsoleConfig
    | FileOutput !FileConfig
    | CustomOutput !(LogEntry -> IO ())
    deriving (Eq)

-- Logging functions
debugLog :: LogLevel -> Text -> DebugLogger -> IO ()
withContext :: LogContext -> DebugLogger -> DebugLogger
formatLogEntry :: LogEntry -> LogFormat -> Text
```

### 4. Trace Collection

```haskell
-- File: src/MCP/Debug/Trace.hs

-- Trace types
data TraceCollector = TraceCollector
    { buffer :: !(TQueue TraceEvent)
    , config :: !TraceConfig
    , export :: !ExportConfig
    } deriving (Eq)

data TraceEvent = TraceEvent
    { eventType :: !EventType
    , timestamp :: !UTCTime
    , data_ :: !Value
    , source :: !Text
    } deriving (Eq, Show)

-- Tracing functions
startTrace :: TraceConfig -> IO TraceCollector
recordEvent :: TraceEvent -> TraceCollector -> IO ()
exportTrace :: TraceCollector -> FilePath -> IO ()
```

## Testing Requirements

1. Unit Tests:
   - Message debugging
   - State inspection
   - Log formatting
   - Trace collection

2. Integration Tests:
   - With server/client
   - Multiple debuggers
   - Different outputs

3. Performance Tests:
   - Debug overhead
   - Logging impact
   - Trace collection

## Files to Create/Modify
1. `src/MCP/Debug/Message.hs` - Message debugger
2. `src/MCP/Debug/State.hs` - State inspector
3. `src/MCP/Debug/Log.hs` - Logging utilities
4. `test/MCP/Debug/DebugSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - time
  - stm
  - yaml
```

## Acceptance Criteria
1. Message debugging works
2. State inspection functional
3. Logging complete
4. Trace collection working
5. Performance acceptable
6. Documentation clear
7. Easy to use
8. Code passes checks

## Non-Goals
1. Complex profiling
2. Performance analysis
3. Security auditing
4. Production monitoring

## Resources
1. Debugging Patterns
2. Logging Best Practices
3. Trace Analysis Examples