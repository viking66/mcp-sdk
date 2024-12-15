# Task: Implement JSON-RPC Message Encoding/Decoding

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core JSON-RPC message types (001-base-message-types)

## Context
After implementing the core JSON-RPC message types, we need to provide reliable JSON serialization and deserialization for all message types. This encoding/decoding must be type-safe, efficient, and compliant with the JSON-RPC 2.0 specification.

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── JsonRpc/
│               ├── Encoding.hs     -- Core encoding/decoding
│               ├── Internal/
│               │   ├── Parser.hs   -- Parsing internals
│               │   └── Builder.hs  -- JSON building internals
│               └── Instances/
│                   ├── Request.hs  -- Request instances
│                   ├── Response.hs -- Response instances
│                   └── Error.hs    -- Error instances
└── test/
    └── MCP/
        └── Core/
            └── JsonRpc/
                ├── EncodingSpec.hs    -- Encoding tests
                ├── Instances/         -- Instance tests
                └── TestVectors/       -- Specification tests
                    ├── Request/
                    ├── Response/
                    └── Error/
```

## Requirements
1. Implement ToJSON/FromJSON instances for all core message types
2. Ensure proper handling of null, undefined, and empty values
3. Maintain type safety during encoding/decoding
4. Provide helpful error messages for decoding failures
5. Handle all JSON-RPC 2.0 message formats correctly
6. Meet performance benchmarks (specified below)
7. Achieve 90% code coverage

## Detailed Implementation Plan

### 1. Core Encoding Module

```haskell
-- File: src/MCP/Core/JsonRpc/Encoding.hs

module MCP.Core.JsonRpc.Encoding
    ( -- * Encoding Functions
      encodeMessage
    , encodeMessageStrict
    , encodeMessageBuilder
      -- * Decoding Functions
    , decodeMessage
    , decodeMessageStrict
    , decodeMessageWithError
      -- * Type Classes
    , JsonRpcEncodable(..)
    , JsonRpcDecodable(..)
    ) where

import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.ByteString.Builder as Builder

-- | Efficient encoding of JSON-RPC messages
class JsonRpcEncodable a where
    encodeJsonRpc :: a -> Builder.Builder
    default encodeJsonRpc :: ToJSON a => a -> Builder.Builder
    encodeJsonRpc = Aeson.fromEncoding . toEncoding

-- | Safe decoding of JSON-RPC messages
class JsonRpcDecodable a where
    decodeJsonRpc :: ByteString -> Either DecodeError a
    default decodeJsonRpc :: FromJSON a => ByteString -> Either DecodeError a
    decodeJsonRpc = first DecodeError . eitherDecodeStrict
```

### 2. Performance-Optimized Instances

```haskell
-- File: src/MCP/Core/JsonRpc/Instances/Request.hs

{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O2 #-}

module MCP.Core.JsonRpc.Instances.Request where

import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

instance ToJSON a => ToJSON (Request a) where
    -- Use direct Builder creation for performance
    toEncoding Request{..} = Aeson.pairs $
        "jsonrpc" .= ("2.0" :: Text) <>
        "method"  .= requestMethod <>
        "params"  .= requestParams <>
        "id"      .= requestId

    -- Provide optimized toJSON for tools that need it
    toJSON = ...

instance FromJSON a => FromJSON (Request a) where
    -- Use custom parser for performance
    parseJSON = withObject "Request" $ \obj -> do
        ver <- obj .:! "jsonrpc" .!= "2.0"
        unless (ver == "2.0") $
            fail "Invalid JSON-RPC version"
        Request ver
            <$> obj .: "method"
            <*> obj .:? "params"
            <*> obj .: "id"
```

### 3. Performance Requirements

```haskell
benchmarks :: [Benchmark]
benchmarks = 
    [ bgroup "encoding"
        [ bench "small request" $ nf encodeMessage smallReq
        , bench "large request" $ nf encodeMessage largeReq
        ]
    , bgroup "decoding"
        [ bench "small request" $ nf decodeMessage smallJson
        , bench "large request" $ nf decodeMessage largeJson
        ]
    ]

requirements =
    [ ("small message encode", <5μs)
    , ("small message decode", <5μs)
    , ("large message encode", <50μs)
    , ("large message decode", <50μs)
    ]
```

## Testing Requirements

### 1. Property Tests
```haskell
prop_roundtrip :: Request a -> Property
prop_roundtrip req =
    decode (encode req) === Right req

prop_error_handling :: DecodeError -> Property
prop_error_handling err =
    decodeMessageWithError (errorJson err) === Left err
```

### 2. Vector Tests
```haskell
testVectors :: TestTree
testVectors = testGroup "JSON-RPC Spec Vectors"
    [ testCase "request vectors" $ do
        forM_ requestVectors $ \(json, expected) ->
            decode json @?= Right expected
    , testCase "response vectors" $ ...
    , testCase "error vectors" $ ...
    ]
```

## Dependencies
```yaml
dependencies:
  -- Core
  - aeson ^>= 2.1
  - bytestring ^>= 0.11
  - text ^>= 2.0
  - scientific ^>= 0.3
  - unordered-containers ^>= 0.2

  -- Performance
  - ghc-prim ^>= 0.9
  - primitive ^>= 0.7
  - vector ^>= 0.13

  -- Testing
  - criterion ^>= 1.6
  - tasty-golden ^>= 2.3
  - quickcheck-instances ^>= 0.3
```

## Acceptance Criteria
1. All instances implemented and passing tests
2. Performance benchmarks met
3. 90% code coverage achieved
4. Zero memory leaks verified
5. All JSON-RPC spec vectors passing
6. Documentation complete
7. Code passes style checks

## Non-Goals
1. Streaming JSON
2. Custom JSON formats
3. Schema validation
4. Protocol extensions

## Resources
1. [JSON-RPC Specification](https://www.jsonrpc.org/specification)
2. [Aeson Performance Guide](https://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#g:7)
3. [GHC Optimization Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html)