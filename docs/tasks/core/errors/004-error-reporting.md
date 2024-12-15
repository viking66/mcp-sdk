# Task: Implement Error Reporting Utilities

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error hierarchy (001-error-types)
- Transport errors (002-transport-errors)
- Protocol errors (003-protocol-errors)

## Context
We need to implement utilities for error reporting, formatting, and aggregation. These utilities will provide consistent error handling patterns across the MCP SDK and help developers understand and handle errors effectively.

## Requirements
1. Create error formatting utilities
2. Implement error aggregation
3. Add error reporting hooks
4. Create error context utilities
5. Support structured error output

## Detailed Implementation Plan

### 1. Error Reporting Types

```haskell
-- File: src/MCP/Core/Error/Report.hs

-- Core reporting types
data ErrorReport = ErrorReport
    { errorType :: !ErrorType
    , message :: !Text
    , context :: !ErrorContext
    , trace :: ![ErrorTraceEntry]
    , timestamp :: !UTCTime
    , metadata :: !(Map Text Value)
    } deriving (Show)

data ErrorType
    = TransportErrorType
    | ProtocolErrorType
    | ApplicationErrorType
    | SystemErrorType
    deriving (Show, Eq)

data ErrorTraceEntry = ErrorTraceEntry
    { location :: !Text
    , context :: !Text
    , timestamp :: !UTCTime
    } deriving (Show)

-- Error reporting configuration
data ErrorReportConfig = ErrorReportConfig
    { includeTrace :: !Bool
    , includeContext :: !Bool
    , maxTraceDepth :: !Int
    , formatters :: ![ErrorFormatter]
    } deriving (Show)

-- Error formatting
type ErrorFormatter = ErrorReport -> Text
```

### 2. Reporting Functions

```haskell
-- Core reporting functions
reportError :: MonadIO m => ErrorReport -> m ()
reportErrorWith :: MonadIO m => ErrorReportConfig -> ErrorReport -> m ()

-- Error report construction
mkErrorReport :: McpError -> ErrorContext -> IO ErrorReport
mkErrorReportWith :: ErrorReportConfig -> McpError -> ErrorContext -> IO ErrorReport

-- Report aggregation
aggregateReports :: [ErrorReport] -> ErrorReport
filterReports :: (ErrorReport -> Bool) -> [ErrorReport] -> [ErrorReport]
```

### 3. Formatting Utilities

```haskell
-- Error formatting
formatError :: ErrorReport -> Text
formatErrorJson :: ErrorReport -> Value
formatErrorStructured :: ErrorReport -> [(Text, Text)]

-- Custom formatters
withLocation :: ErrorFormatter -> ErrorFormatter
withTimestamp :: ErrorFormatter -> ErrorFormatter
withContext :: ErrorFormatter -> ErrorFormatter
```

### 4. Context Utilities

```haskell
-- Context handling
addContext :: Text -> Value -> ErrorContext -> ErrorContext
withContext :: ErrorContext -> (ErrorContext -> a) -> a
modifyContext :: (ErrorContext -> ErrorContext) -> ErrorReport -> ErrorReport

-- Trace handling
addTrace :: Text -> ErrorReport -> IO ErrorReport
withTrace :: Text -> IO a -> IO a
```

## Testing Requirements

1. Property Tests:
   - Error formatting
   - Report aggregation
   - Context handling

2. Unit Tests:
   - All utility functions
   - Formatting options
   - Context manipulation

3. Integration Tests:
   - Error reporting flow
   - Format combinations
   - Context propagation

## Files to Create/Modify
1. `src/MCP/Core/Error/Report.hs` - Core reporting
2. `src/MCP/Core/Error/Format.hs` - Formatting utilities
3. `src/MCP/Core/Error/Context.hs` - Context utilities
4. `test/MCP/Core/Error/ReportSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - time
  - text
  - containers
  - mtl
```

## Acceptance Criteria
1. Error reporting system works
2. Formatting options implemented
3. Context system working
4. Report aggregation functional
5. Full test coverage
6. Documentation complete
7. Example usage code
8. Code passes style checks

## Non-Goals
1. Implementing logging system
2. Building error recovery
3. Implementing monitoring
4. Supporting external reporting systems

## Resources
1. Error Handling Best Practices
2. JSON Error Format Standards