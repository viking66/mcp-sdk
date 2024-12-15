# Task: Implement Stdio Transport

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-server
**Prerequisites**: 
- Server initialization (001-initialization)
- Base Transport typeclass (mcp-core/transport/001-transport-typeclass)
- Stream handling types (mcp-core/transport/002-stream-types)
- Message framing (mcp-core/transport/004-message-framing)

## Context
We need to implement a transport that uses standard input/output for communication between the MCP server and clients. This transport needs to handle message framing, buffering, and proper stream management while being robust against various edge cases.

## Requirements
1. Define stdio transport types
2. Implement stream handling
3. Create message framing
4. Support error handling
5. Enable proper cleanup

## Detailed Implementation Plan

### 1. Stdio Transport Types

```haskell
-- File: src/MCP/Server/Transport/Stdio.hs

-- Core transport type
data StdioTransport = StdioTransport
    { inStream :: !InputStream
    , outStream :: !OutputStream
    , config :: !StdioConfig
    , state :: !(TVar TransportState)
    } deriving (Eq)

-- Stream types
data InputStream = InputStream
    { handle :: !Handle
    , buffer :: !Buffer
    , decoder :: !TextDecoder
    } deriving (Eq)

data OutputStream = OutputStream
    { handle :: !Handle
    , buffer :: !Buffer
    , encoder :: !TextEncoder
    } deriving (Eq)

-- Configuration
data StdioConfig = StdioConfig
    { bufferSize :: !Int
    , encoding :: !TextEncoding
    , lineEnding :: !LineEnding
    , flushMode :: !FlushMode
    } deriving (Eq, Show)
```

### 2. Stream Management

```haskell
-- Stream operations
data StreamOperation
    = ReadOperation !ReadRequest
    | WriteOperation !WriteRequest
    deriving (Eq, Show)

data ReadRequest = ReadRequest
    { minBytes :: !Int
    , maxBytes :: !Int
    , timeout :: !NominalDiffTime
    } deriving (Eq, Show)

data WriteRequest = WriteRequest
    { content :: !ByteString
    , flush :: !Bool
    } deriving (Eq, Show)

-- Stream functions
readInput :: ReadRequest -> InputStream -> IO (Either TransportError ByteString)
writeOutput :: WriteRequest -> OutputStream -> IO (Either TransportError ())
flushOutput :: OutputStream -> IO (Either TransportError ())
```

### 3. Message Framing

```haskell
-- Frame handling
data StdioFrame = StdioFrame
    { header :: !FrameHeader
    , content :: !ByteString
    , checksum :: !Word32
    } deriving (Eq, Show)

data FrameHeader = FrameHeader
    { version :: !Word8
    , flags :: !Word8
    , length :: !Word32
    } deriving (Eq, Show)

-- Frame operations
readFrame :: InputStream -> IO (Either TransportError StdioFrame)
writeFrame :: StdioFrame -> OutputStream -> IO (Either TransportError ())
validateFrame :: StdioFrame -> Either TransportError ()
```

### 4. Transport Implementation

```haskell
instance Transport StdioTransport where
    -- Initialize transport
    initialize config transport = do
        setupStreams transport
        initializeFraming transport
        pure ()

    -- Connect transport
    connect transport = do
        validateStreams transport
        startMessageLoop transport
        pure ()

    -- Send message
    send message transport = do
        frame <- encodeMessage message
        writeFrame frame (outStream transport)

    -- Receive message
    receive transport = do
        frame <- readFrame (inStream transport)
        decodeMessage frame

    -- Close transport
    close transport = do
        flushOutput (outStream transport)
        cleanupStreams transport
        pure ()
```

## Testing Requirements

1. Property Tests:
   - Frame encoding/decoding
   - Message handling
   - Buffer operations

2. Unit Tests:
   - Stream operations
   - Transport lifecycle
   - Error scenarios

3. Integration Tests:
   - Full message cycle
   - Error handling
   - Resource cleanup

## Files to Create/Modify
1. `src/MCP/Server/Transport/Stdio.hs` - Main transport implementation
2. `src/MCP/Server/Transport/Frame.hs` - Frame handling
3. `src/MCP/Server/Transport/Buffer.hs` - Buffer management
4. `test/MCP/Server/Transport/StdioSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - bytestring
  - text
  - stm
  - async
```

## Acceptance Criteria
1. Transport implemented
2. Stream handling working
3. Framing functional
4. Error handling complete
5. Resource cleanup reliable
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Encryption support
2. Complex buffering
3. Compression
4. Multiple channels

## Resources
1. MCP Transport Specification
2. Stdio Handling Patterns
3. Framing Examples