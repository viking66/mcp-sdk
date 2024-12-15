# Task: Implement MCP Protocol-Specific Message Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core JSON-RPC message types (001-base-message-types)
- JSON encoding/decoding (002-json-encoding)
- Message validation (003-message-validation)

## Context
We need to define the specific message types used by the Model Context Protocol (MCP) that build on our base JSON-RPC implementation. These types represent the actual protocol messages exchanged between MCP clients and servers.

## Project Structure
```
mcp-core/
├── src/
│   └── MCP/
│       └── Core/
│           └── Protocol/
│               ├── Types.hs           -- Core protocol types
│               ├── Messages/
│               │   ├── Initialize.hs  -- Initialization messages
│               │   ├── Resource.hs    -- Resource messages
│               │   └── Tool.hs        -- Tool messages
│               ├── Version/
│               │   ├── Types.hs       -- Version types
│               │   └── Compat.hs      -- Version compatibility
│               └── Internal/
│                   ├── Constants.hs   -- Protocol constants
│                   └── Builders.hs    -- Message builders
└── test/
    └── MCP/
        └── Core/
            └── Protocol/
                ├── TypesSpec.hs       -- Type tests
                ├── Messages/          -- Message tests
                ├── Version/           -- Version tests
                └── TestVectors/       -- Protocol test vectors
```

## Requirements
1. Define core protocol messages
2. Implement version handling
3. Create capabilities system
4. Support message validation
5. Enable message construction
6. Meet performance targets
7. Achieve 95% code coverage

## Detailed Implementation Plan

### 1. Core Protocol Types

```haskell
-- File: src/MCP/Core/Protocol/Types.hs

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module MCP.Core.Protocol.Types
    ( -- * Versions
      Version(..)
    , VersionRange
    , SemVer(..)
      -- * Capabilities
    , Capabilities(..)
    , CapabilitySet
      -- * Messages
    , InitMessage(..)
    , ResourceMessage(..)
    , ToolMessage(..)
    ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Protocol version with semantic versioning
data Version = Version
    { major :: !Word  -- ^ Major version
    , minor :: !Word  -- ^ Minor version
    , patch :: !Word  -- ^ Patch version
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass (NFData)

-- | Server capabilities with bitset optimization
newtype Capabilities = Capabilities
    { capabilityBits :: Word32 }
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

-- Capability bits for O(1) checks
pattern HasResources = Capabilities 0x1
pattern HasPrompts   = Capabilities 0x2
pattern HasTools     = Capabilities 0x4
pattern HasSampling  = Capabilities 0x8
```

### 2. Message Type Safety

```haskell
-- File: src/MCP/Core/Protocol/Messages/Initialize.hs

-- | Type-safe initialization messages
data InitMessage
    = InitRequest !InitializeParams
    | InitResponse !InitializeResult
    | Initialized
    deriving (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Prevent invalid state transitions
data InitPhase
    = NotInitialized
    | Initializing !InitContext
    | Initialized !ServerInfo
    deriving (Eq, Show)

-- | Type-safe state transitions
initializeServer 
    :: InitPhase          -- ^ Current phase
    -> InitMessage        -- ^ Incoming message
    -> Except InitError InitPhase  -- ^ New phase
```

### 3. Performance Optimizations

```haskell
{-# OPTIONS_GHC -O2 -fspec-constr #-}

-- | Fast capability checking
hasCapability :: Capabilities -> Capability -> Bool
hasCapability (Capabilities bits) cap =
    bits .&. capabilityBit cap /= 0
{-# INLINE hasCapability #-}

-- | Optimized message construction
mkInitRequest :: MonadIO m 
              => ClientInfo 
              -> Capabilities 
              -> m InitMessage
mkInitRequest client caps = do
    -- Allocate on the heap once
    let !params = InitializeParams
            { clientInfo = client
            , capabilities = caps
            , trace = Nothing
            }
    pure $! InitRequest params
{-# SPECIALIZE mkInitRequest :: ClientInfo -> Capabilities -> IO InitMessage #-}
```

## Performance Requirements

```haskell
benchmarks :: [Benchmark]
benchmarks = 
    [ bench "version compare" $ nf compareVersions versions
    , bench "capability check" $ nf checkCapabilities caps
    , bench "message construct" $ nfIO constructMessage
    ]

requirements =
    [ ("version operations", <1μs)
    , ("capability checks", <100ns)
    , ("message creation", <10μs)
    ]
```

## Testing Requirements

### 1. Property Tests
```haskell
prop_version_ordering vs =
    sort vs === sortBy compareVersion vs

prop_capability_combining cs =
    combineCapabilities cs === 
    foldl' (.&.) defaultCapabilities cs
```

### 2. Protocol Tests
```haskell
protocolTests = testGroup "Protocol Messages"
    [ testCase "init sequence" testInitSequence
    , testCase "resource usage" testResourceUsage
    , testCase "tool execution" testToolExecution
    ]
```

## Dependencies
```yaml
dependencies:
  -- Core
  - aeson ^>= 2.1
  - text ^>= 2.0
  - bytestring ^>= 0.11
  
  -- Performance
  - primitive ^>= 0.7
  - deepseq ^>= 1.4
  
  -- Types
  - these ^>= 1.1
  - some ^>= 1.0
  - constraints ^>= 0.13
```

## Acceptance Criteria
1. Message types implemented
2. Version handling working
3. Capabilities functional
4. Performance targets met
5. 95% code coverage
6. Memory usage optimized
7. Documentation complete
8. Code passes checks

## Non-Goals
1. Message routing
2. Connection handling
3. State persistence
4. Custom protocols

## Resources
1. [MCP Protocol Specification](protocol-spec.md)
2. [Semantic Versioning](https://semver.org)
3. [GHC Optimization Guide](ghc-optimization.md)