# Task: Implement Tool Result Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core tool types (001-tool-types)
- Tool schema types (002-schema-types)

## Context
We need to implement types for handling tool execution results, including success and error cases, progress reporting, and result metadata. The system needs to support both synchronous and asynchronous results, with proper type safety and error handling.

## Requirements
1. Define result type hierarchy
2. Implement progress tracking
3. Create error result types
4. Support result metadata
5. Enable result validation

## Detailed Implementation Plan

### 1. Core Result Types

```haskell
-- File: src/MCP/Core/Tool/Result.hs

-- Base result type
data ToolResult
    = SuccessResult !Success
    | ErrorResult !Error
    | ProgressResult !Progress
    | CancelledResult
    deriving (Eq, Show)

-- Success result
data Success = Success
    { resultValue :: !ResultValue
    , metadata :: !ResultMetadata
    , timing :: !ExecutionTiming
    } deriving (Eq, Show)

-- Result values
data ResultValue
    = TextValue !Text
    | JsonValue !Value
    | BinaryValue !ByteString
    | StreamValue !ResultStream
    | VoidValue
    deriving (Eq, Show)

-- Result metadata
data ResultMetadata = ResultMetadata
    { contentType :: !(Maybe Text)
    , encoding :: !(Maybe Text)
    , size :: !(Maybe Integer)
    , checksum :: !(Maybe Text)
    , tags :: ![Text]
    } deriving (Eq, Show)
```

### 2. Progress Tracking

```haskell
-- Progress reporting
data Progress = Progress
    { completed :: !Integer
    , total :: !Integer
    , status :: !ProgressStatus
    , details :: !(Maybe Text)
    , timestamp :: !UTCTime
    } deriving (Eq, Show)

data ProgressStatus
    = Starting
    | Running !Double  -- Percentage complete
    | Paused
    | Cleanup
    deriving (Eq, Show)

-- Progress updates
updateProgress :: Progress -> ToolResult -> IO ToolResult
computeProgress :: Integer -> Integer -> Progress
```

### 3. Error Handling

```haskell
-- Error results
data Error = Error
    { errorCode :: !ErrorCode
    , message :: !Text
    , details :: !(Maybe Value)
    , recoverable :: !Bool
    , suggestion :: !(Maybe Text)
    } deriving (Eq, Show)

data ErrorCode
    = ValidationError
    | ExecutionError
    | TimeoutError
    | ResourceError
    | PermissionError
    | SystemError
    deriving (Eq, Show)

-- Error creation
mkError :: ErrorCode -> Text -> Error
withDetails :: Value -> Error -> Error
withSuggestion :: Text -> Error -> Error
```

### 4. Result Processing

```haskell
-- Result processing
data ResultProcessor a = ResultProcessor
    { onSuccess :: Success -> IO a
    , onError :: Error -> IO a
    , onProgress :: Progress -> IO ()
    , onCancel :: IO a
    }

-- Stream handling
data ResultStream = ResultStream
    { streamId :: !UUID
    , chunkSize :: !Int
    , contentType :: !Text
    , readChunk :: IO (Maybe ByteString)
    }

-- Processing functions
processResult :: ResultProcessor a -> ToolResult -> IO a
collectResult :: ToolResult -> IO ByteString
transformResult :: (ResultValue -> IO ResultValue) -> ToolResult -> IO ToolResult
```

## Testing Requirements

1. Property Tests:
   - Result construction
   - Progress calculation
   - Error handling

2. Unit Tests:
   - All result types
   - Progress updates
   - Error scenarios

3. Integration Tests:
   - With tool execution
   - Stream handling
   - Async results

## Files to Create/Modify
1. `src/MCP/Core/Tool/Result.hs` - Core result types
2. `src/MCP/Core/Tool/Progress.hs` - Progress tracking
3. `src/MCP/Core/Tool/Error.hs` - Error handling
4. `test/MCP/Core/Tool/ResultSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - time
  - uuid
  - bytestring
```

## Acceptance Criteria
1. Result types implemented
2. Progress tracking working
3. Error handling complete
4. Stream support functional
5. Processing utilities work
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Result persistence
2. Complex streaming
3. Retry handling
4. Result caching

## Resources
1. MCP Tool Result Specification
2. Error Handling Patterns
3. Progress Reporting Best Practices