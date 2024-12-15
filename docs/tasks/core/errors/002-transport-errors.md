# Task: Implement Transport-Specific Error Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error hierarchy (001-error-types)

## Context
We need to implement type-safe error handling for transport-specific failures in MCP communication. The system must handle errors from different transport types (stdio, SSE) while providing detailed error information and efficient error handling paths.

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── Transport/
│               ├── Error/
│               │   ├── Types.hs        -- Core transport errors
│               │   ├── Network.hs      -- Network errors
│               │   └── IO.hs           -- IO errors
│               ├── Context/
│               │   ├── Types.hs        -- Context types
│               │   └── Metrics.hs      -- Error metrics
│               └── Internal/
│                   ├── Recovery.hs     -- Recovery logic
│                   └── Classification.hs -- Error classification
└── test/
    └── MCP/
        └── Core/
            └── Transport/
                ├── Error/
                │   ├── TypesSpec.hs    -- Error tests
                │   └── NetworkSpec.hs  -- Network tests
                └── TestVectors/        -- Error scenarios
```

## Requirements
1. Implement transport errors
2. Support error context
3. Enable error recovery
4. Meet performance targets
5. Support metrics collection
6. Achieve 95% coverage

## Detailed Implementation Plan

### 1. Type-Safe Transport Errors

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module MCP.Core.Transport.Error.Types
    ( -- * Error Types
      TransportError(..)
    , NetworkError(..)
    , IOError(..)
      -- * Error Construction
    , mkTransportError
    , fromIOError
    , fromNetworkError
      -- * Error Classification
    , TransportErrorKind(..)
    , classifyError
    ) where

-- | Transport error kinds for type safety
data TransportErrorKind
    = ConnectionK    -- ^ Connection errors
    | NetworkK      -- ^ Network-specific errors
    | IOK           -- ^ IO-related errors
    | TimeoutK      -- ^ Timeout errors
    deriving (Eq, Show)

-- | Transport errors with kind parameter
data TransportError (k :: TransportErrorKind) where
    -- Connection errors
    ConnectFailed      :: !Text -> TransportError 'ConnectionK
    ConnectionClosed   :: !Text -> TransportError 'ConnectionK
    AuthFailed        :: !Text -> TransportError 'ConnectionK

    -- Network errors
    NetworkUnreachable :: !Text -> TransportError 'NetworkK
    InvalidEndpoint    :: !Text -> TransportError 'NetworkK
    TLSError          :: !Text -> TransportError 'NetworkK

    -- IO errors
    ReadError         :: !IOError -> TransportError 'IOK
    WriteError        :: !IOError -> TransportError 'IOK
    
    -- Timeout errors
    OperationTimeout  :: !NominalDiffTime -> TransportError 'TimeoutK
    IdleTimeout       :: !NominalDiffTime -> TransportError 'TimeoutK

deriving instance Eq (TransportError k)
deriving instance Show (TransportError k)
```

### 2. Error Context and Metrics

```haskell
-- | Transport error context
data TransportContext = TransportContext
    { transportInfo :: !TransportInfo   -- ^ Transport details
    , errorMetrics :: !ErrorMetrics    -- ^ Error statistics
    , traceInfo :: !TraceInfo         -- ^ Error trace
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (NFData)

-- | Real-time error metrics
data ErrorMetrics = ErrorMetrics
    { totalErrors :: !Int64           -- ^ Total error count
    , errorsByType :: !(Map Text Int) -- ^ Error distribution
    , lastError :: !UTCTime          -- ^ Last error time
    , recoveryAttempts :: !Int       -- ^ Recovery attempts
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (NFData)

-- | Metrics collection
class HasMetrics e where
    updateMetrics :: e -> ErrorMetrics -> ErrorMetrics
    getErrorRate :: e -> ErrorMetrics -> Double
```

### 3. Performance-Optimized Recovery

```haskell
-- | Fast error recovery logic
data Recovery = Recovery
    { maxAttempts :: !Int
    , backoff :: !BackoffStrategy
    , timeout :: !NominalDiffTime
    } deriving stock (Eq, Show)

-- | Optimized error classification
classifyError :: TransportError k -> ErrorClassification
classifyError = \case
    ConnectFailed{} -> Retryable defaultRecovery
    NetworkUnreachable{} -> Retryable networkRecovery
    TLSError{} -> Fatal
    _ -> NonRetryable
{-# INLINE classifyError #-}

-- | Fast retry decisions
shouldRetry :: TransportError k -> Bool
shouldRetry = \case
    ConnectFailed{} -> True
    NetworkUnreachable{} -> True
    _ -> False
{-# INLINE shouldRetry #-}
```

## Performance Requirements

```haskell
benchmarks :: [Benchmark]
benchmarks = 
    [ bench "error creation" $ nf mkStandardError args
    , bench "error classification" $ nf classifyError err
    , bench "metric update" $ nf updateMetrics metrics
    ]

requirements =
    [ ("error creation", <1μs)
    , ("classification", <100ns)
    , ("metric update", <500ns)
    ]
```

## Testing Requirements

### 1. Property Tests
```haskell
prop_error_classification :: TransportError k -> Property
prop_error_classification err =
    classifyError err === expectedClass err

prop_recovery_strategy :: TransportError k -> Property
prop_recovery_strategy err =
    recoveryFor err === optimalStrategy err
```

### 2. Coverage Goals
- 95% total coverage
- 100% error type coverage
- 100% recovery path coverage

## Dependencies
```yaml
dependencies:
  -- Core
  - network ^>= 3.1
  - tls ^>= 1.5
  - io-streams ^>= 1.5
  
  -- Performance  
  - atomic-primops ^>= 0.8
  - vector ^>= 0.13
  
  -- Testing
  - quickcheck-classes ^>= 0.6
  - tasty-hunit ^>= 0.10
```

## Acceptance Criteria
1. Type-safe error system
2. Performance targets met
3. Coverage goals achieved
4. Recovery logic working
5. Metrics collected
6. Documentation complete
7. Examples provided
8. Code passes checks

## Non-Goals
1. Transport implementation
2. Complex recovery
3. Error persistence
4. Custom protocols

## Resources
1. [Network Programming in Haskell](https://github.com/k-bx/network-programming-in-haskell)
2. [Error Handling Best Practices](https://www.fpcomplete.com/blog/error-handling/)
3. [Performance Optimization Guide](https://wiki.haskell.org/Performance)