# Task: Implement JSON-RPC Message Validation

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core JSON-RPC message types (001-base-message-types)
- JSON encoding/decoding (002-json-encoding)

## Context
We need to implement validation logic for JSON-RPC messages to ensure they conform to the protocol specification. This includes both structural validation and semantic validation of message content, with a focus on performance and clear error reporting.

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── JsonRpc/
│               ├── Validation.hs      -- Core validation
│               ├── Validator/
│               │   ├── Method.hs      -- Method validation
│               │   ├── Params.hs      -- Parameter validation
│               │   └── Error.hs       -- Error code validation
│               └── Internal/
│                   ├── Rules.hs       -- Validation rules
│                   └── Constraints.hs  -- Validation constraints
└── test/
    └── MCP/
        └── Core/
            └── JsonRpc/
                ├── ValidationSpec.hs     -- Core validation tests
                ├── Validator/            -- Validator tests
                └── TestVectors/          -- Protocol test vectors
                    ├── valid/            -- Valid messages
                    └── invalid/          -- Invalid messages
```

## Requirements
1. Implement full JSON-RPC 2.0 validation rules
2. Achieve < 1ms validation time for typical messages
3. Provide detailed, actionable error messages
4. Support batch validation
5. Enable custom validation rules
6. Achieve 95% code coverage

## Detailed Implementation Plan

### 1. Validation System

```haskell
-- File: src/MCP/Core/JsonRpc/Validation.hs

module MCP.Core.JsonRpc.Validation
    ( -- * Validation
      Validator
    , ValidationError(..)
    , validate
    , validateBatch
      -- * Rules
    , ValidationRule
    , makeRule
    , combineRules
      -- * Context
    , ValidationContext
    , withContext
    ) where

import MCP.Core.JsonRpc.Internal.Rules
import qualified Data.Vector as V

-- | Validation monad with error accumulation
newtype Validator a = Validator 
    { runValidator :: ValidationContext -> Writer [ValidationError] a }
    deriving (Functor, Applicative, Monad)

-- | Validation context for stateful validation
data ValidationContext = ValidationContext
    { maxErrors :: !Int
    , path :: ![Text]
    , customRules :: !(Map Text ValidationRule)
    , stats :: !ValidationStats
    } deriving (Eq, Show)

-- | Core validation method
validate :: Validatable a => a -> Either [ValidationError] a
validate = runValidation defaultContext . validateMessage
```

### 2. Validation Rules

```haskell
-- File: src/MCP/Core/JsonRpc/Internal/Rules.hs

module MCP.Core.JsonRpc.Internal.Rules where

import Data.Text.Validate (isControlChar)
import qualified Data.Vector as V

-- | Core method validation rules
methodRules :: [ValidationRule]
methodRules =
    [ noControlChars
    , noRpcPrefix
    , validIdentifier
    ]
  where
    noControlChars = makeRule "no-control-chars" $
        not . any isControlChar

    noRpcPrefix = makeRule "no-rpc-prefix" $
        not . ("rpc." `isPrefixOf`)

-- | Parameter validation with custom rules
validateParams :: Value -> Validator ()
validateParams = \case
    Object obj -> validateObject obj
    Array arr  -> validateArray arr
    _         -> throwError InvalidParamsType
```

### 3. Performance Optimizations

```haskell
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O2 -fspec-constr #-}

-- | Optimized validation for batch messages
validateBatch :: Vector Message -> Either [ValidationError] (Vector Message)
validateBatch = runST $ do
    errors <- VM.new (V.length msgs)
    results <- VM.new (V.length msgs)
    
    -- Parallel validation with error collection
    forConcurrently_ (V.indexed msgs) $ \(i, msg) -> do
        case validate msg of
            Left err -> VM.write errors i (Just err)
            Right r  -> VM.write results i r
            
    -- Check for any errors
    errs <- V.freeze errors
    if V.any isJust errs
        then Left $ V.toList $ V.mapMaybe id errs
        else Right <$> V.freeze results
```

## Performance Requirements

```haskell
benchmarks :: [Benchmark]
benchmarks =
    [ bench "simple message" $ nf validate simpleMsg
    , bench "complex message" $ nf validate complexMsg
    , bench "batch messages" $ nf validateBatch batchMsgs
    ]

requirements =
    [ ("simple validation", <100μs)
    , ("complex validation", <500μs)
    , ("batch validation", <1ms per message)
    ]
```

## Testing Requirements

### 1. Property Tests
```haskell
properties :: TestTree
properties = testGroup "Validation Properties"
    [ testProperty "valid messages pass" validMessagesPass
    , testProperty "invalid messages fail" invalidMessagesFail
    , testProperty "error accumulation" errorAccumulation
    ]
  where
    validMessagesPass msg =
        isValidMessage msg ==> validate msg === Right msg
```

### 2. Edge Cases
```haskell
edgeCases :: TestTree
edgeCases = testGroup "Edge Cases"
    [ testCase "empty method name" $ ...
    , testCase "max length params" $ ...
    , testCase "unicode method names" $ ...
    ]
```

## Dependencies
```yaml
dependencies:
  -- Core
  - text ^>= 2.0
  - vector ^>= 0.13
  - containers ^>= 0.6
  - mtl ^>= 2.3
  
  -- Validation
  - validation-selective ^>= 0.1
  
  -- Performance
  - primitive ^>= 0.7
  - parallel ^>= 3.2
```

## Acceptance Criteria
1. All spec validation rules implemented
2. Performance targets met
3. 95% code coverage achieved
4. Memory usage within bounds
5. Clear error messages
6. Property tests passing
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Schema validation
2. Content validation
3. Custom protocols
4. Runtime rule changes

## Resources
1. [JSON-RPC Specification](https://www.jsonrpc.org/specification)
2. [Validation Patterns](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-some-haskell-error-handling-approaches)