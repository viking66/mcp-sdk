# Task: Implement Core Error Type Hierarchy

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: None

## Context
We need to define a comprehensive error type hierarchy for the MCP SDK that covers all error cases from protocol errors to runtime failures. The system needs to be type-safe, efficient, and provide detailed error reporting while maintaining good performance.

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── Error/
│               ├── Types.hs          -- Core error types
│               ├── Context.hs        -- Error context
│               ├── Convert.hs        -- Error conversion
│               ├── Domain/
│               │   ├── Protocol.hs   -- Protocol errors
│               │   ├── Transport.hs  -- Transport errors
│               │   └── Resource.hs   -- Resource errors
│               └── Internal/
│                   ├── Stack.hs      -- Error stack traces
│                   └── Pretty.hs     -- Error formatting
└── test/
    └── MCP/
        └── Core/
            └── Error/
                ├── TypesSpec.hs      -- Type tests
                ├── Domain/           -- Domain tests
                └── TestVectors/      -- Error scenarios
```

## Requirements
1. Define type-safe error hierarchy
2. Support structured error context
3. Enable error conversion
4. Provide error utilities
5. Support error aggregation
6. Meet performance targets
7. Achieve 95% code coverage

## Detailed Implementation Plan

### 1. Core Error System

```haskell
-- File: src/MCP/Core/Error/Types.hs

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.Core.Error.Types 
    ( -- * Core Types
      McpError(..)
    , ErrorKind(..)
    , ErrorSource(..)
      -- * Error Construction
    , mkError
    , withContext
    , fromException
      -- * Error Handling
    , catchError
    , handleError
    , tryError
    ) where

import Control.Exception (Exception(..), SomeException)
import GHC.Records (HasField)
import GHC.TypeLits (Symbol)

-- | Core error type with phantom kind
data McpError (k :: ErrorKind) = McpError
    { errorType :: !Text
    , errorCode :: !Int32
    , message :: !Text
    , details :: !Value
    , source :: !ErrorSource
    , context :: !ErrorContext
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (NFData)

-- | Error classification for type safety
data ErrorKind
    = ProtocolK    -- ^ Protocol-level errors
    | TransportK   -- ^ Transport-level errors
    | ResourceK    -- ^ Resource-related errors
    | ApplicationK -- ^ Application-level errors
    deriving (Eq, Show)

-- | Error source tracking
data ErrorSource = ErrorSource
    { component :: !Text      -- ^ Component name
    , location :: !SrcLoc     -- ^ Source location
    , timestamp :: !UTCTime   -- ^ Error time
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (NFData)
```

### 2. Type-Safe Error Construction

```haskell
-- | Type-safe error construction
class HasErrorKind e where
    type ErrorKindOf e :: ErrorKind
    toError :: e -> McpError (ErrorKindOf e)

-- | Smart constructor with context
mkError :: (HasErrorKind e, MonadIO m) 
        => e 
        -> Maybe Value 
        -> m (McpError (ErrorKindOf e))
mkError err details = do
    src <- mkErrorSource
    ctx <- getErrorContext
    pure McpError
        { errorType = errorTypeOf err
        , errorCode = errorCodeOf err
        , message = errorMessageOf err
        , details = fromMaybe Null details
        , source = src
        , context = ctx
        }

-- | Error context capture
captureContext :: MonadIO m => m ErrorContext
captureContext = do
    tid <- myThreadId
    callStack <- getCallStack
    pure ErrorContext
        { threadId = tid
        , callStack = callStack
        , environment = []
        }
```

### 3. Error Handling

```haskell
-- | Core error monad
newtype ErrorT e m a = ErrorT 
    { runErrorT :: m (Either (McpError e) a) }
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)

-- | Error handling type class
class MonadError e m where
    throwError :: McpError e -> m a
    catchError :: m a -> (McpError e -> m a) -> m a

-- | Handle errors with recovery
handleError :: MonadError e m 
            => (McpError e -> m a) 
            -> m a 
            -> m a
handleError h m = catchError m h

-- | Convert exceptions to errors
fromException :: Exception e => e -> McpError 'ApplicationK
fromException e = McpError
    { errorType = "exception"
    , errorCode = exceptionErrorCode
    , message = Text.pack $ show e
    , details = Null
    , source = defaultSource
    , context = defaultContext
    }
```

## Performance Requirements

```haskell
benchmarks :: [Benchmark]
benchmarks = 
    [ bench "error construction" $ nf mkStandardError args
    , bench "context capture" $ nfIO captureContext
    , bench "error handling" $ nf handleTestError input
    ]

requirements =
    [ ("error creation", <5μs)
    , ("context capture", <10μs)
    , ("error handling", <1μs)
    ]
```

## Testing Requirements

### 1. Property Tests
```haskell
prop_error_conversion :: McpError e -> Property
prop_error_conversion err =
    fromError (toError err) === Right err

prop_error_handling :: Testable e => e -> Property
prop_error_handling e =
    forAll genHandler $ \h ->
        handleError h (throwError e) === h e
```

### 2. Coverage Requirements
- 95% total coverage
- 100% coverage of error constructors
- 100% coverage of error conversions

## Dependencies
```yaml
dependencies:
  -- Core
  - exceptions ^>= 0.10
  - transformers ^>= 0.5
  - mtl ^>= 2.3
  
  -- Performance
  - deepseq ^>= 1.4
  - primitive ^>= 0.7
  
  -- Testing
  - hedgehog ^>= 1.1
  - hspec ^>= 2.10
```

## Acceptance Criteria
1. Type-safe error system
2. Performance targets met
3. Coverage goals achieved
4. Memory usage optimized
5. Error handling robust
6. Documentation complete
7. Examples provided
8. Code passes checks

## Non-Goals
1. Error recovery
2. Error persistence
3. Error analysis
4. Custom error types

## Resources
1. [Error Handling in Haskell](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
2. [Type-Safe Error Handling](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html)
3. [Exception Best Practices](https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/)