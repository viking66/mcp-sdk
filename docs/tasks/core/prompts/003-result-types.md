# Task: Implement Prompt Result Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core prompt types (001-prompt-types)
- Prompt template types (002-template-types)

## Context
We need to implement types for handling prompt generation results, including success and error cases, rendering information, and result metadata. The system needs to handle both immediate and deferred results, with proper validation and error reporting.

## Requirements
1. Define result type hierarchy
2. Implement rendering metadata
3. Create error result types
4. Support result validation
5. Enable result processing

## Detailed Implementation Plan

### 1. Core Result Types

```haskell
-- File: src/MCP/Core/Prompt/Result.hs

-- Core result type
data PromptResult
    = CompletedResult !CompletedPrompt
    | FailedResult !PromptError
    | DeferredResult !DeferredPrompt
    deriving (Eq, Show)

-- Completed result
data CompletedPrompt = CompletedPrompt
    { content :: !PromptContent
    , metadata :: !ResultMetadata
    , timing :: !RenderTiming
    } deriving (Eq, Show)

-- Content types
data PromptContent
    = TextContent !Text
    | StructuredContent !Value
    | MultipartContent ![PromptPart]
    deriving (Eq, Show)

-- Result metadata
data ResultMetadata = ResultMetadata
    { arguments :: !(Map Text Value)
    , renderedTemplate :: !Text
    , variablesUsed :: ![Text]
    , formatVersion :: !Text
    } deriving (Eq, Show)
```

### 2. Deferred Results

```haskell
-- Deferred result handling
data DeferredPrompt = DeferredPrompt
    { resultId :: !UUID
    , status :: !DeferredStatus
    , estimatedCompletion :: !(Maybe UTCTime)
    } deriving (Eq, Show)

data DeferredStatus
    = Queued
    | Processing !Double  -- Progress percentage
    | Ready
    | Failed !PromptError
    deriving (Eq, Show)

-- Deferred operations
checkStatus :: DeferredPrompt -> IO DeferredStatus
awaitResult :: DeferredPrompt -> IO PromptResult
cancelDeferred :: DeferredPrompt -> IO ()
```

### 3. Error Handling

```haskell
-- Error types
data PromptError
    = TemplateError !TemplateError
    | ArgumentError !ArgumentError
    | ValidationError !ValidationError
    | RenderError !RenderError
    | SystemError !Text
    deriving (Eq, Show)

data ArgumentError
    = MissingArgument !Text
    | InvalidArgument !Text !Text  -- name, reason
    | UnknownArgument !Text
    deriving (Eq, Show)

data RenderError
    = TimeoutError !NominalDiffTime
    | ResourceError !Text
    | FormatError !Text
    deriving (Eq, Show)

-- Error utilities
isRecoverable :: PromptError -> Bool
getErrorContext :: PromptError -> Map Text Text
formatError :: PromptError -> Text
```

### 4. Result Processing

```haskell
-- Processing types
data ResultProcessor a = ResultProcessor
    { onCompleted :: CompletedPrompt -> IO a
    , onFailed :: PromptError -> IO a
    , onDeferred :: DeferredPrompt -> IO a
    }

-- Timing information
data RenderTiming = RenderTiming
    { startTime :: !UTCTime
    , endTime :: !UTCTime
    , phases :: ![PhaseTime]
    } deriving (Eq, Show)

data PhaseTime = PhaseTime
    { phase :: !Text
    , duration :: !NominalDiffTime
    } deriving (Eq, Show)

-- Processing functions
processResult :: ResultProcessor a -> PromptResult -> IO a
mapResult :: (PromptContent -> IO PromptContent) -> PromptResult -> IO PromptResult
validateResult :: PromptResult -> Either PromptError ()
```

## Testing Requirements

1. Property Tests:
   - Result construction
   - Error handling
   - Metadata validation

2. Unit Tests:
   - All result types
   - Deferred handling
   - Processing functions

3. Integration Tests:
   - With prompt system
   - Error scenarios
   - Timing accuracy

## Files to Create/Modify
1. `src/MCP/Core/Prompt/Result.hs` - Core result types
2. `src/MCP/Core/Prompt/Error.hs` - Error handling
3. `src/MCP/Core/Prompt/Processing.hs` - Result processing
4. `test/MCP/Core/Prompt/ResultSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - text
  - aeson
  - time
  - uuid
```

## Acceptance Criteria
1. Result types implemented
2. Error handling complete
3. Deferred results working
4. Processing utilities functional
5. Timing accurate
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Result persistence
2. Complex transformations
3. Result caching
4. Retry handling

## Resources
1. MCP Prompt Specification
2. Error Handling Patterns
3. Time Measurement Best Practices