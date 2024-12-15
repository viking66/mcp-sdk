# Task: Implement Tool Execution

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-client
**Prerequisites**: 
- Client initialization (001-initialization)
- Core tool types (mcp-core/tools/001-tool-types)
- Tool schema types (mcp-core/tools/002-schema-types)

## Context
We need to implement client-side tool execution for MCP. This includes tool discovery, input validation, execution, and result handling. The implementation should provide a clean API for tool usage while handling various error conditions.

## Requirements
1. Define tool execution types
2. Implement tool discovery
3. Create execution handling
4. Support input validation
5. Enable result processing

## Detailed Implementation Plan

### 1. Tool Execution Types

```haskell
-- File: src/MCP/Client/Tool/Executor.hs

-- Core executor types
data ToolExecutor = ToolExecutor
    { client :: !Client
    , config :: !ExecutorConfig
    , registry :: !(TVar ToolRegistry)
    , stats :: !(TVar ExecutorStats)
    } deriving (Eq)

data ExecutorConfig = ExecutorConfig
    { timeout :: !NominalDiffTime
    , validateInput :: !Bool
    , retryPolicy :: !RetryPolicy
    , collectMetrics :: !Bool
    } deriving (Eq, Show)

data ExecuteRequest = ExecuteRequest
    { tool :: !Tool
    , input :: !Value
    , options :: !ExecuteOptions
    } deriving (Eq, Show)
```

### 2. Tool Discovery

```haskell
-- Discovery types
data ToolRegistry = ToolRegistry
    { tools :: !(Map ToolId Tool)
    , categories :: !(Map Text [ToolId])
    , lastUpdate :: !UTCTime
    } deriving (Eq, Show)

data ToolFilter = ToolFilter
    { category :: !(Maybe Text)
    , capabilities :: ![ToolCapability]
    , inputSchema :: !(Maybe InputSchema)
    } deriving (Eq, Show)

-- Discovery functions
listTools :: Client -> IO [Tool]
findTool :: ToolId -> Client -> IO (Maybe Tool)
searchTools :: ToolFilter -> Client -> IO [Tool]
refreshRegistry :: Client -> IO ()
```

### 3. Execution Handling

```haskell
-- Execution types
data ExecutionContext = ExecutionContext
    { requestId :: !RequestId
    , startTime :: !UTCTime
    , timeout :: !NominalDiffTime
    , metadata :: !Map Text Value
    } deriving (Eq, Show)

data ExecuteOptions = ExecuteOptions
    { validateSchema :: !Bool
    , collectMetrics :: !Bool
    , retryCount :: !Int
    } deriving (Eq, Show)

-- Execution functions
executeTool :: ExecuteRequest -> ToolExecutor -> IO ToolResult
validateInput :: Value -> Tool -> IO (Either ValidationError ())
monitorExecution :: ExecutionContext -> IO () -> IO ExecutionMetrics
```

### 4. Result Processing

```haskell
-- Result handling
data ExecutionResult = ExecutionResult
    { result :: !ToolResult
    , metrics :: !ExecutionMetrics
    , context :: !ExecutionContext
    } deriving (Eq, Show)

data ExecutionMetrics = ExecutionMetrics
    { duration :: !NominalDiffTime
    , retries :: !Int
    , validationTime :: !NominalDiffTime
    } deriving (Eq, Show)

-- Processing functions
processResult :: ExecutionResult -> (ToolResult -> IO a) -> IO a
collectMetrics :: ExecutionMetrics -> ToolExecutor -> IO ()
handleError :: ToolError -> ExecutionContext -> IO ToolResult
```

## Testing Requirements

1. Unit Tests:
   - Tool discovery
   - Input validation
   - Execution flow
   - Result handling

2. Integration Tests:
   - Server interaction
   - Tool execution
   - Error scenarios

3. Property Tests:
   - Input validation
   - Result processing
   - Metric collection

## Files to Create/Modify
1. `src/MCP/Client/Tool/Executor.hs` - Tool executor
2. `src/MCP/Client/Tool/Discovery.hs` - Tool discovery
3. `src/MCP/Client/Tool/Result.hs` - Result handling
4. `test/MCP/Client/Tool/ExecutorSpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - aeson
  - time
  - retry
```

## Acceptance Criteria
1. Tool execution working
2. Discovery functional
3. Input validation complete
4. Result handling working
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex tool chaining
2. Tool state management
3. Advanced scheduling
4. Tool versioning

## Resources
1. MCP Tool Specification
2. Execution Patterns
3. Error Handling Examples