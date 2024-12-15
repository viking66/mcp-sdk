# Task: Implement Basic Resource Serving

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-server
**Prerequisites**: 
- Server initialization (001-initialization)
- Core resource types (mcp-core/resources/001-resource-types)
- Resource content types (mcp-core/resources/002-content-types)

## Context
We need to implement the basic resource serving functionality for MCP servers. This includes handling resource registration, content serving, and basic resource requests. The implementation should support different content types while maintaining proper error handling.

## Requirements
1. Define resource handler types
2. Implement resource registration
3. Create content serving logic
4. Support request handling
5. Enable basic validation

## Detailed Implementation Plan

### 1. Resource Handler Types

```haskell
-- File: src/MCP/Server/Resource/Handler.hs

-- Core handler types
data ResourceHandler = ResourceHandler
    { handlerId :: !HandlerId
    , patterns :: ![ResourcePattern]
    , handler :: !ResourceCallback
    , config :: !HandlerConfig
    } deriving (Eq)

-- Resource callback type
type ResourceCallback = ResourceRequest -> IO (Either ResourceError ResourceResponse)

-- Handler configuration
data HandlerConfig = HandlerConfig
    { maxSize :: !(Maybe Integer)
    , supportedTypes :: ![MimeType]
    , cacheControl :: !CacheControl
    , timeout :: !NominalDiffTime
    } deriving (Eq, Show)

-- Resource pattern matching
data ResourcePattern
    = ExactMatch !Text
    | PrefixMatch !Text
    | RegexMatch !Text
    | CustomMatch !(Text -> Bool)
    deriving (Eq)
```

### 2. Request Handling

```haskell
-- Request types
data ResourceRequest = ResourceRequest
    { requestId :: !RequestId
    , resourceUri :: !ResourceUri
    , requestType :: !RequestType
    , headers :: !Headers
    , context :: !RequestContext
    } deriving (Eq, Show)

data RequestType
    = ListResources !(Maybe Text)  -- Optional prefix
    | ReadResource !ReadOptions
    | HeadResource
    deriving (Eq, Show)

data ReadOptions = ReadOptions
    { range :: !(Maybe Range)
    , ifMatch :: !(Maybe ETag)
    , ifNoneMatch :: !(Maybe ETag)
    } deriving (Eq, Show)

-- Response types
data ResourceResponse
    = SingleResource !Resource !Content
    | MultipleResources ![Resource]
    | NotModified !ETag
    | NoContent
    deriving (Eq, Show)
```

### 3. Registration System

```haskell
-- Registration types
data HandlerRegistration = HandlerRegistration
    { handler :: !ResourceHandler
    , priority :: !Priority
    , metadata :: !HandlerMetadata
    } deriving (Eq, Show)

data HandlerMetadata = HandlerMetadata
    { description :: !(Maybe Text)
    , tags :: ![Text]
    , examples :: ![ResourceExample]
    } deriving (Eq, Show)

-- Registration functions
registerHandler :: HandlerRegistration -> Server -> IO (Either ResourceError HandlerId)
unregisterHandler :: HandlerId -> Server -> IO ()
updateHandler :: HandlerId -> HandlerRegistration -> Server -> IO (Either ResourceError ())
```

### 4. Content Serving

```haskell
-- Content serving types
data ContentServing = ContentServing
    { content :: !Content
    , metadata :: !ContentMetadata
    , transfer :: !TransferMode
    } deriving (Eq, Show)

data TransferMode
    = FullTransfer
    | ChunkedTransfer !ChunkSize
    | StreamingTransfer !StreamConfig
    deriving (Eq, Show)

-- Serving functions
serveContent :: ContentServing -> ResourceResponse -> IO (Either ResourceError ())
validateContent :: Content -> HandlerConfig -> Either ResourceError ()
transformContent :: Content -> TransformRule -> IO (Either ResourceError Content)
```

## Testing Requirements

1. Property Tests:
   - Pattern matching
   - Content serving
   - Handler registration

2. Unit Tests:
   - Request handling
   - Response generation
   - Error scenarios

3. Integration Tests:
   - Full request cycle
   - Multiple handlers
   - Content types

## Files to Create/Modify
1. `src/MCP/Server/Resource/Handler.hs` - Handler types
2. `src/MCP/Server/Resource/Request.hs` - Request handling
3. `src/MCP/Server/Resource/Content.hs` - Content serving
4. `test/MCP/Server/Resource/HandlerSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - bytestring
  - http-types
  - mime-types
  - filepath
```

## Acceptance Criteria
1. Handler types implemented
2. Registration working
3. Request handling functional
4. Content serving complete
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex caching
2. Content transformation
3. Resource versioning
4. Access control

## Resources
1. MCP Resource Specification
2. Content Serving Patterns
3. Handler Examples