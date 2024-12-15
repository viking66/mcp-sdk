# Task: Implement Resource Content Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core resource types (001-resource-types)

## Context
We need to implement specialized content type handling for MCP resources. This includes handling different content formats, providing serialization/deserialization, and managing content transformations. The implementation should be extensible and type-safe.

## Requirements
1. Define content type hierarchy
2. Implement content serialization
3. Add content validation
4. Support content transformations
5. Handle streaming content

## Detailed Implementation Plan

### 1. Content Type System

```haskell
-- File: src/MCP/Core/Resource/Content.hs

-- Core content type
data Content
    = TextContent !TextContent
    | BinaryContent !BinaryContent
    | JsonContent !JsonContent
    | StreamContent !StreamContent
    deriving (Eq, Show)

-- Text content
data TextContent = TextContent
    { textValue :: !Text
    , textEncoding :: !TextEncoding
    , lineEnding :: !LineEnding
    } deriving (Eq, Show)

-- Binary content
data BinaryContent = BinaryContent
    { binaryData :: !ByteString
    , binaryFormat :: !BinaryFormat
    } deriving (Eq, Show)

-- JSON content
data JsonContent = JsonContent
    { jsonValue :: !Value
    , jsonFormat :: !JsonFormat
    } deriving (Eq, Show)

-- Stream content
data StreamContent = StreamContent
    { streamSource :: !StreamSource
    , streamFormat :: !StreamFormat
    , chunkSize :: !Int
    } deriving (Eq, Show)
```

### 2. Content Format Types

```haskell
data TextEncoding
    = UTF8
    | UTF16LE
    | UTF16BE
    | ASCII
    deriving (Eq, Show)

data LineEnding
    = LF
    | CRLF
    | CR
    deriving (Eq, Show)

data BinaryFormat
    = Raw
    | Base64
    | Hex
    deriving (Eq, Show)

data JsonFormat
    = Compact
    | Pretty
    | Canonical
    deriving (Eq, Show)

data StreamFormat
    = TextStream !TextEncoding
    | BinaryStream !BinaryFormat
    | JsonStream !JsonFormat
    deriving (Eq, Show)
```

### 3. Content Operations

```haskell
-- Content creation
mkTextContent :: Text -> IO Content
mkBinaryContent :: ByteString -> IO Content
mkJsonContent :: Value -> IO Content
mkStreamContent :: StreamSource -> IO Content

-- Content conversion
contentToText :: Content -> IO Text
contentToBinary :: Content -> IO ByteString
contentToJson :: Content -> IO Value
contentToStream :: Content -> IO StreamSource

-- Content validation
validateContent :: Content -> Either ContentError ()
validateTextContent :: TextContent -> Either ContentError ()
validateBinaryContent :: BinaryContent -> Either ContentError ()
validateJsonContent :: JsonContent -> Either ContentError ()
```

### 4. Streaming Support

```haskell
data StreamSource
    = FileStream !FilePath
    | SocketStream !Socket
    | CustomStream !(IO ByteString)
    deriving (Eq)

-- Streaming operations
streamContent :: StreamSource -> (ByteString -> IO ()) -> IO ()
collectStream :: StreamSource -> IO ByteString
transformStream :: (ByteString -> ByteString) -> StreamSource -> StreamSource
```

## Testing Requirements

1. Property Tests:
   - Content conversions
   - Format handling
   - Validation logic

2. Unit Tests:
   - All content types
   - Stream operations
   - Format conversions

3. Integration Tests:
   - With resource types
   - Stream handling
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Core/Resource/Content.hs` - Core content types
2. `src/MCP/Core/Resource/Format.hs` - Format handling
3. `src/MCP/Core/Resource/Stream.hs` - Stream support
4. `test/MCP/Core/Resource/ContentSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - text
  - bytestring
  - aeson
  - streaming
  - base64
```

## Acceptance Criteria
1. Content types implemented
2. Format handling working
3. Stream support functional
4. Conversions reliable
5. Validation complete
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex content transformations
2. Content compression
3. Content encryption
4. Format-specific optimizations

## Resources
1. Text Encoding Standards
2. JSON Format Specifications
3. Streaming Best Practices