# Task: Implement Simple Tool Execution

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-server
**Prerequisites**: 
- Server initialization (001-initialization)
- Core tool types (mcp-core/tools/001-tool-types)
- Tool input schema types (mcp-core/tools/002-schema-types)
- Tool result types (mcp-core/tools/003-result-types)

## Context
We need to implement basic tool execution functionality for MCP servers. This includes handling tool registration, input validation, execution, and result handling. The implementation should support synchronous execution while maintaining proper error handling.

## Requirements
1. Define tool executor types
2. Implement tool registration
3. Create execution logic
4. Support input validation
5. Enable result handling

## Detailed Implementation Plan

### 1. Tool Executor Types

```haskell
-- File: src/MCP/Server/Tool/Executor.hs

-- Core executor types
data ToolExecutor = ToolExecutor
    { executorId :: !ExecutorId
    , tool :: !Tool
    , executor :: !ExecutorCallback
    , config :: !ExecutorConfig
    } deriving (Eq)

-- Executor callback type
type ExecutorCallback = ToolInput -> IO (Either ToolError ToolResult)

-- Executor configuration
data ExecutorConfig = ExecutorConfig
    { timeout :: !NominalDiffTime
    , maxRetries :: !Int
    , validateInput :: !Bool
    , collectMetrics :: !Bool
    } deriving (Eq, Show)

-- Execution context
data ExecutionContext = ExecutionContext
    { requestId :: !RequestId
    , sessionId :: !SessionId
    , startTime :: !UTCTime
    , metadata :: !Map Text Value
    } deriving (Eq, Show)
```

### 2. Tool Registration

```haskell
-- Registration types
data ToolRegistration = ToolRegistration
    { executor :: !ToolExecutor
    , priority :: !Priority
    , metadata :: !ExecutorMetadata
    } deriving (Eq, Show)

data ExecutorMetadata = ExecutorMetadata
    { description :: !(Maybe Text)
    , examples :: ![ToolExample]
    , tags :: ![Text]
    } deriving (Eq, Show)

-- Registration functions
registerTool :: ToolRegistration -> Server -> IO (Either ToolError ExecutorId)
unregisterTool :: ExecutorId -> Server -> IO ()
updateTool :: ExecutorId -> ToolRegistration -> Server -> IO (Either ToolError ())
```

### 3. Execution Logic

```haskell
-- Execution types
data ToolExecution = ToolExecution
    { input :: !ToolInput
    , context :: !ExecutionContext
    , config :: !ExecutionConfig
    } deriving (Eq, Show)

data ExecutionConfig = ExecutionConfig
    { validateInput :: !Bool
    , collectMetrics :: !Bool
    , timeout :: !NominalDiffTime
    } deriving (Eq, Show)

-- Execution functions
executeTool :: ToolExecution -> ToolExecutor -> IO (Either ToolError ToolResult)
validateExecution :: ToolExecution -> Either ToolError ()
collectMetrics :: ToolExecution -> ToolResult -> IO ExecutionMetrics
```

### 4. Result Handling

```haskell
-- Result types
data ExecutionResult = ExecutionResult
    { result :: !ToolResult
    , metrics :: !ExecutionMetrics
    , context :: !ExecutionContext
    } deriving (Eq, Show)

data ExecutionMetrics = ExecutionMetrics
    { duration :: !NominalDiffTime
    , peakMemory :: !(Maybe Integer)
    , errorCount :: !Int
    } deriving (Eq, Show)

-- Result functions
processResult :: ExecutionResult -> IO (Either ToolError ToolResponse)
validateResult :: ToolResult -> Tool -> Either ToolError ()
formatResponse :: ToolResult -> ToolResponse
```

## Testing Requirements

1. Property Tests:
   - Input validation
   - Result handling
   - Metric collection

2. Unit Tests:
   - Execution flow
   - Error handling
   - Configuration options

3. Integration Tests:
   - Full execution cycle
   - Multiple tools
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Server/Tool/Executor.hs` - Executor types
2. `src/MCP/Server/Tool/Registration.hs` - Registration logic
3. `src/MCP/Server/Tool/Execution.hs` - Execution handling
4. `test/MCP/Server/Tool/ExecutorSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - time
  - async
  - stm
```

## Acceptance Criteria
1. Executor types implemented
2. Registration working
3. Execution functional
4. Result handling complete
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Asynchronous execution
2. Complex scheduling
3. State persistence
4. Tool chaining

## Resources
1. MCP Tool Specification
2. Execution Patterns
3. Error Handling Examples