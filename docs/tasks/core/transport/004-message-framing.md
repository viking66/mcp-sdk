# Task: Implement Message Framing Interface

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Base Transport typeclass (001-transport-typeclass)
- Stream handling types (002-stream-types)
- Connection lifecycle (003-connection-lifecycle)

## Context
We need to implement a message framing interface that handles the packaging, boundaries, and reassembly of messages across the transport layer. This needs to work with different transport mechanisms while maintaining message integrity and providing proper error handling.

## Requirements
1. Define message framing types
2. Implement frame encoding/decoding
3. Create frame boundary handling
4. Support different framing strategies
5. Handle framing errors

## Detailed Implementation Plan

### 1. Core Framing Types

```haskell
-- File: src/MCP/Core/Transport/Framing.hs

-- Core frame type
data Frame = Frame
    { frameHeader :: !FrameHeader
    , framePayload :: !ByteString
    , frameChecksum :: !Checksum
    } deriving (Eq, Show)

data FrameHeader = FrameHeader
    { frameVersion :: !Word8
    , frameType :: !FrameType
    , payloadLength :: !Int32
    , flags :: !FrameFlags
    } deriving (Eq, Show)

data FrameType
    = DataFrame
    | ControlFrame
    | ErrorFrame
    deriving (Eq, Show)

newtype FrameFlags = FrameFlags Word8
    deriving (Eq, Show, Bits)

newtype Checksum = Checksum Word32
    deriving (Eq, Show)
```

### 2. Framing Interface

```haskell
-- Framing strategy typeclass
class Framing f where
    -- Frame a message
    frameMessage :: ByteString -> f -> IO Frame
    
    -- Extract message from frame
    unframeMessage :: Frame -> f -> IO ByteString
    
    -- Frame validation
    validateFrame :: Frame -> f -> IO (Either FramingError ())
    
    -- Optional methods with defaults
    computeChecksum :: Frame -> f -> IO Checksum
    splitFrames :: ByteString -> f -> IO [Frame]
```

### 3. Frame Processing

```haskell
data FrameProcessor = FrameProcessor
    { processorConfig :: !FrameProcessorConfig
    , incomingFrames :: !(TQueue Frame)
    , outgoingFrames :: !(TQueue Frame)
    , stats :: !(TVar FrameStats)
    }

data FrameProcessorConfig = FrameProcessorConfig
    { maxFrameSize :: !Int
    , checksumType :: !ChecksumType
    , frameTimeout :: !NominalDiffTime
    } deriving (Show)

-- Frame processing functions
processIncomingFrame :: Frame -> FrameProcessor -> IO (Either FramingError ByteString)
prepareOutgoingFrame :: ByteString -> FrameProcessor -> IO Frame
validateFrameHeader :: FrameHeader -> IO (Either FramingError ())
validateChecksum :: Frame -> IO Bool
```

### 4. Error Handling

```haskell
data FramingError
    = InvalidFrameHeader !Text
    | InvalidChecksum !Checksum !Checksum  -- Expected, Got
    | FrameTooLarge !Int !Int             -- Got, Max
    | IncompleteFrame !Int !Int           -- Got, Expected
    | InvalidFrameType !Word8
    | FrameTimeout !NominalDiffTime
    deriving (Show)

-- Error handling functions
handleFramingError :: FramingError -> FrameProcessor -> IO ()
recoverFromFramingError :: FramingError -> FrameProcessor -> IO ()
```

## Testing Requirements

1. Property Tests:
   - Frame encoding/decoding
   - Checksum validation
   - Frame boundary detection

2. Unit Tests:
   - Frame processing
   - Error handling
   - Different frame types

3. Integration Tests:
   - With stream handling
   - With different transports
   - Error recovery

## Files to Create/Modify
1. `src/MCP/Core/Transport/Framing.hs` - Core framing types
2. `src/MCP/Core/Transport/Frame.hs` - Frame implementation
3. `src/MCP/Core/Transport/Checksum.hs` - Checksum handling
4. `test/MCP/Core/Transport/FramingSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - bytestring
  - stm
  - cryptonite
  - memory
```

## Acceptance Criteria
1. Framing interface implemented
2. Frame processing working
3. Checksum validation complete
4. Error handling robust
5. Different frame types supported
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Implementing encryption
2. Complex compression
3. Custom checksum algorithms
4. Protocol-specific framing

## Resources
1. Message Framing Best Practices
2. Checksum Implementation Guides
3. Binary Protocol Design Patterns