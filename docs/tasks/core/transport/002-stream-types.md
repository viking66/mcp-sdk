# Task: Implement Stream Handling Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Base Transport typeclass (001-transport-typeclass)
- Core error types (001-error-types)

## Context
We need to implement types for managing streaming data through our transport layer. These types will handle both synchronous and asynchronous streaming, with support for backpressure and resource cleanup.

## Requirements
1. Define stream types for reading/writing
2. Implement stream lifecycle management
3. Support backpressure handling
4. Add chunking and buffering capabilities
5. Enable resource cleanup and monitoring

## Detailed Implementation Plan

### 1. Core Stream Types

```haskell
-- File: src/MCP/Core/Transport/Stream.hs

-- Core stream types
data Stream = Stream
    { streamId :: !StreamId
    , bufferSize :: !Int
    , encoding :: !StreamEncoding
    , direction :: !StreamDirection
    , status :: !(TVar StreamStatus)
    , buffer :: !(TQueue ByteString)
    } deriving (Eq)

newtype StreamId = StreamId { unStreamId :: UUID }
    deriving (Eq, Show, Ord)

data StreamDirection
    = ReadStream
    | WriteStream
    | BiDirectional
    deriving (Eq, Show)

data StreamStatus
    = StreamOpen
    | StreamClosed
    | StreamError !StreamError
    deriving (Eq, Show)

data StreamEncoding
    = RawEncoding
    | TextEncoding !TextEncoding
    | JsonEncoding
    deriving (Eq, Show)
```

### 2. Stream Operations

```haskell
-- Stream lifecycle
openStream :: StreamConfig -> IO Stream
closeStream :: Stream -> IO ()
withStream :: StreamConfig -> (Stream -> IO a) -> IO a

-- Reading operations
readChunk :: Stream -> IO ByteString
readExactly :: Int -> Stream -> IO ByteString
readUntil :: ByteString -> Stream -> IO ByteString
readTimeout :: NominalDiffTime -> Stream -> IO (Maybe ByteString)

-- Writing operations
writeChunk :: ByteString -> Stream -> IO ()
writeChunks :: [ByteString] -> Stream -> IO ()
flush :: Stream -> IO ()

-- Status and control
isStreamOpen :: Stream -> IO Bool
getStreamStatus :: Stream -> IO StreamStatus
resetStream :: Stream -> IO ()
```

### 3. Stream Configuration

```haskell
data StreamConfig = StreamConfig
    { configBufferSize :: !Int
    , configEncoding :: !StreamEncoding
    , configDirection :: !StreamDirection
    , configTimeout :: !NominalDiffTime
    , configChunkSize :: !Int
    } deriving (Eq, Show)

-- Smart constructor
mkStreamConfig :: StreamDirection -> StreamConfig
defaultStreamConfig :: StreamConfig
```

### 4. Buffer Management

```haskell
data Buffer = Buffer
    { bufferData :: !(TQueue ByteString)
    , bufferSize :: !Int
    , bytesRead :: !IORef Int64
    , bytesWritten :: !IORef Int64
    } deriving (Eq)

-- Buffer operations
newBuffer :: Int -> IO Buffer
readBuffer :: Buffer -> IO ByteString
writeBuffer :: Buffer -> ByteString -> IO ()
flushBuffer :: Buffer -> IO [ByteString]
isBufferFull :: Buffer -> IO Bool
```

## Testing Requirements

1. Property Tests:
   - Stream operations
   - Buffer management
   - Encoding handling

2. Unit Tests:
   - Stream lifecycle
   - Read/write operations
   - Status management

3. Integration Tests:
   - Stream with transports
   - Error handling
   - Resource cleanup

## Files to Create/Modify
1. `src/MCP/Core/Transport/Stream.hs` - Core stream types
2. `src/MCP/Core/Transport/Buffer.hs` - Buffer management
3. `src/MCP/Core/Transport/Encoding.hs` - Encoding handling
4. `test/MCP/Core/Transport/StreamSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - uuid
  - bytestring
  - async
  - time
```

## Acceptance Criteria
1. Stream types defined and working
2. Buffer management implemented
3. Encoding support functional
4. Backpressure handling works
5. Resource cleanup reliable
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Implementing specific transports
2. Complex stream transformations
3. Advanced compression
4. SSL/TLS handling

## Resources
1. STM Documentation
2. ByteString Best Practices
3. Haskell Streaming Patterns