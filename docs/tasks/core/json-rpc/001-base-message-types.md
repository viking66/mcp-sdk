# Task: Implement Core JSON-RPC Message Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Dependencies**: None

## Context
We need to implement the foundational types for JSON-RPC 2.0 message handling in our MCP SDK. These types will be used throughout the codebase for all protocol communication. The implementation needs to be type-safe, efficient, and compliant with the JSON-RPC 2.0 specification.

## Requirements
1. Implement core message types according to [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
2. Ensure type safety through Haskell's type system
3. Support proper error handling and validation
4. Enable easy construction of valid messages
5. Prevent construction of invalid messages at compile time where possible
6. Achieve 90% code coverage
7. Meet performance benchmarks for encoding/decoding

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── JsonRpc/
│               ├── Types.hs        -- Core message types
│               ├── Error.hs        -- Error types and utilities
│               ├── Validation.hs   -- Validation functions
│               └── Internal/
│                   ├── Utils.hs    -- Internal utilities
│                   └── Constants.hs -- Protocol constants
└── test/
    └── MCP/
        └── Core/
            └── JsonRpc/
                ├── TypesSpec.hs       -- Type tests
                ├── ErrorSpec.hs       -- Error tests
                ├── ValidationSpec.hs   -- Validation tests
                └── Vectors/           -- Test vectors
                    ├── Request.json
                    ├── Response.json
                    └── Error.json
```

## Detailed Implementation Plan

### 1. Core Types to Implement

```haskell
-- File: src/MCP/Core/JsonRpc/Types.hs

module MCP.Core.JsonRpc.Types 
    ( -- * Core Types
      RequestId(..)
    , Request(..)
    , Response(..)
    , Notification(..)
      -- * Smart Constructors
    , mkRequest
    , mkResponse
    , mkNotification
      -- * Type Classes
    , ToJsonRpc(..)
    , FromJsonRpc(..)
    ) where

import MCP.Core.Error.Types (Error)

-- Request ID can be number, string, or null
data RequestId 
    = NumberId !Integer
    | StringId !Text
    | NullId
    deriving stock (Eq, Show)
    deriving anyclass (NFData)

-- A JSON-RPC Request
data Request a = Request
    { requestJsonrpc :: !Text        -- Must be "2.0"
    , requestMethod :: !Text         -- Method name
    , requestParams :: !(Maybe a)    -- Optional parameters
    , requestId :: !RequestId        -- Request identifier
    } deriving stock (Eq, Show, Functor)
      deriving anyclass (NFData)

-- Similar definitions for Response and Notification
```

### 2. Error Types

```haskell
-- File: src/MCP/Core/JsonRpc/Error.hs

module MCP.Core.JsonRpc.Error
    ( JsonRpcError(..)
    , ErrorCode(..)
    , mkError
    , isProtocolError
    , isApplicationError
    ) where

import MCP.Core.Error.Types (Error)

-- Standard error codes with documentation
pattern ParseError :: ErrorCode
pattern ParseError = ErrorCode (-32700)
{-# COMPLETE ParseError #-}
```

### 3. Type Classes and Validation

```haskell
class ToJsonRpc a where
    toJsonRpc :: a -> Value
    default toJsonRpc :: (Generic a, GToJson Zero a) => a -> Value
    toJsonRpc = genericToJsonRpc

class FromJsonRpc a where
    fromJsonRpc :: Value -> Either Error a
    default fromJsonRpc :: (Generic a, GFromJson Zero a) => Value -> Either Error a
    fromJsonRpc = genericFromJsonRpc
```

## Testing Requirements

### 1. Property Tests
- Message roundtrip properties (encoding/decoding)
- Validation properties
- Error handling properties
- Coverage target: 90%

### 2. Specification Tests
- All JSON-RPC 2.0 test vectors
- Edge cases from specification
- Error conditions from specification

### 3. Performance Tests
```haskell
benchmarks :: [Benchmark]
benchmarks =
    [ bench "encode small message" $ nf encode smallMessage
    , bench "decode small message" $ nf decode smallMessage
    , bench "validate request" $ nf validateRequest request
    ]

requirements =
    [ ("small message encode", <10μs)
    , ("small message decode", <10μs)
    , ("message validation", <1μs)
    ]
```

## Dependencies
```yaml
dependencies:
  -- Core functionality
  - aeson ^>= 2.1
  - text ^>= 2.0
  - scientific ^>= 0.3
  - deepseq ^>= 1.4
  - generic-lens ^>= 2.2

  -- Testing
  - hedgehog ^>= 1.1
  - hspec ^>= 2.10
  - quickcheck-instances ^>= 0.3
  - criterion ^>= 1.6
  - tasty-golden ^>= 2.3

  -- Development
  - hspec-discover ^>= 2.10
```

## Acceptance Criteria
1. All types implemented with proper instances
2. 90% code coverage achieved
3. Performance benchmarks met
4. All specification tests passing
5. Documentation complete with examples
6. No HLint warnings
7. Passes fourmolu formatting
8. All property tests passing

## Non-Goals
1. Protocol-specific message types
2. Transport implementation
3. Server/client implementations
4. Advanced error recovery

## Resources
1. [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
2. [Test Vectors](https://www.jsonrpc.org/specification/JSON-RPC-2-0-test-vectors.json)
3. [Aeson Documentation](https://hackage.haskell.org/package/aeson)