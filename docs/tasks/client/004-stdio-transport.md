# Task: Implement Stdio Transport for Client

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-client
**Prerequisites**: 
- Client initialization (001-initialization)
- Core transport types (mcp-core/transport/001-transport-typeclass)
- Message framing (mcp-core/transport/004-message-framing)

## Context
We need to implement the client-side stdio transport for MCP. This transport needs to manage standard input/output streams for communication with MCP servers, handle message framing, and provide proper error handling.

## Requirements
1. Define stdio transport types
2. Implement stream management
3. Create message handling
4. Support error handling
5. Enable transport lifecycle

## Detailed Implementation Plan

### 1. Transport Types

```haskell
-- File: src/MCP/Client/Transport/Stdio.hs

-- Core transport type
data StdioTransport = StdioTransport
    { config :: !StdioConfig
    , process :: !ProcessHandle
    , inStream :: !InputStream
    , outStream :: !OutputStream
    , state :: !(TVar TransportState)
    } deriving (Eq)

-- Configuration
data StdioConfig = StdioConfig
    { command :: !FilePath
    , arguments :: ![String]
    , environment :: !(Maybe [(String, String)])
    , workingDir :: !(Maybe FilePath)
    , encoding :: !TextEncoding
    } deriving (Eq, Show)

-- Stream types
data InputStream = InputStream
    { handle :: !Handle
    , buffer :: !IORef ByteString
    , decoder :: !TextDecoder
    } deriving (Eq)

data OutputStream = OutputStream
    { handle :: !Handle
    , buffer :: !IORef ByteString
    , encoder :: !TextEncoder
    } deriving (Eq)
```

### 2. Process Management

```haskell
-- Process handling
data ProcessConfig = ProcessConfig
    { createFlags :: ![ProcessFlag]
    , priority :: !(Maybe ProcessPriority)
    , groupId :: !(Maybe ProcessGroupId)
    } deriving (Eq, Show)

data ProcessFlag
    = Detached
    | NewSession
    | InheritHandles
    deriving (Eq, Show)

-- Process functions
startProcess :: StdioConfig -> ProcessConfig -> IO ProcessHandle
stopProcess :: ProcessHandle -> IO ()
monitorProcess :: ProcessHandle -> IO ProcessStatus
```

### 3. Message Handling

```haskell
-- Message types
data StdioMessage = StdioMessage
    { header :: !MessageHeader
    , content :: !ByteString
    , checksum :: !Word32
    } deriving (Eq, Show)

data MessageHeader = MessageHeader
    { version :: !Word8
    , flags :: !MessageFlags
    , length :: !Word32
    } deriving (Eq, Show)

-- Message functions
readMessage :: InputStream -> IO (Either TransportError StdioMessage)
writeMessage :: StdioMessage -> OutputStream -> IO (Either TransportError ())
validateMessage :: StdioMessage -> Either TransportError ()
```

### 4. Transport Implementation

```haskell
instance Transport StdioTransport where
    -- Initialize transport
    initialize config transport = do
        process <- startProcess (transportConfig config)
        streams <- setupStreams process
        setupBuffering streams
        pure ()
        
    -- Connect transport
    connect transport = do
        validateProcess (process transport)
        initializeStreams transport
        startMessageLoop transport
        
    -- Send message
    send message transport = do
        encoded <- encodeMessage message
        writeMessage encoded (outStream transport)
        
    -- Receive message
    receive transport = do
        message <- readMessage (inStream transport)
        decodeMessage message
        
    -- Close transport
    close transport = do
        cleanupStreams transport
        stopProcess (process transport)
```

## Testing Requirements

1. Unit Tests:
   - Process handling
   - Stream operations
   - Message formatting
   - Error handling

2. Integration Tests:
   - With server processes
   - Error scenarios
   - Cleanup behavior

3. Property Tests:
   - Message encoding/decoding
   - Stream properties
   - Process management

## Files to Create/Modify
1. `src/MCP/Client/Transport/Stdio.hs` - Main transport
2. `src/MCP/Client/Transport/Process.hs` - Process handling
3. `src/MCP/Client/Transport/Stream.hs` - Stream management
4. `test/MCP/Client/Transport/StdioSpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - process
  - stm
  - bytestring
  - async
```

## Acceptance Criteria
1. Transport implemented
2. Process handling working
3. Stream management functional
4. Message handling complete
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex IPC
2. Process supervision
3. Stream compression
4. Process monitoring

## Resources
1. MCP Transport Specification
2. Process Management Patterns
3. Stream Handling Examples