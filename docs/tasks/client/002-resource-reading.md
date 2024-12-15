# Task: Implement Resource Reading

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-client
**Prerequisites**: 
- Client initialization (001-initialization)
- Core resource types (mcp-core/resources/001-resource-types)

## Context
We need to implement the client-side resource reading functionality for MCP. This includes handling resource discovery, content retrieval, and proper error handling. The implementation should support different content types and provide a clean API for resource access.

## Requirements
1. Define resource reading types
2. Implement resource discovery
3. Create content retrieval
4. Support content validation
5. Enable error handling

## Detailed Implementation Plan

### 1. Resource Reading Types

```haskell
-- File: src/MCP/Client/Resource/Reader.hs

-- Core reader types
data ResourceReader = ResourceReader
    { client :: !Client
    , config :: !ReaderConfig
    , cache :: !(TVar ResourceCache)
    , stats :: !(TVar ReaderStats)
    } deriving (Eq)

data ReaderConfig = ReaderConfig
    { timeout :: !NominalDiffTime
    , maxSize :: !(Maybe Integer)
    , cacheEnabled :: !Bool
    , validateContent :: !Bool
    } deriving (Eq, Show)

data ReadRequest = ReadRequest
    { uri :: !ResourceUri
    , options :: !ReadOptions
    , context :: !RequestContext
    } deriving (Eq, Show)
```

### 2. Resource Discovery

```haskell
-- Discovery types
data ResourceDiscovery = ResourceDiscovery
    { pattern :: !ResourcePattern
    , recursive :: !Bool
    , filter :: !(Maybe ResourceFilter)
    } deriving (Eq, Show)

data ResourceFilter = ResourceFilter
    { mimeTypes :: ![MimeType]
    , maxSize :: !(Maybe Integer)
    , modifiedSince :: !(Maybe UTCTime)
    } deriving (Eq, Show)

-- Discovery functions
listResources :: ResourcePattern -> Client -> IO [Resource]
findResource :: ResourceUri -> Client -> IO (Maybe Resource)
searchResources :: ResourceFilter -> Client -> IO [Resource]
```

### 3. Content Retrieval

```haskell
-- Content handling
data ContentRequest = ContentRequest
    { resource :: !Resource
    , range :: !(Maybe Range)
    , transform :: !(Maybe ContentTransform)
    } deriving (Eq, Show)

data ContentTransform
    = Decode !ContentDecoder
    | Convert !ContentConverter
    | Chain ![ContentTransform]
    deriving (Eq, Show)

-- Retrieval functions
readContent :: ContentRequest -> ResourceReader -> IO Content
streamContent :: ContentRequest -> (Content -> IO ()) -> ResourceReader -> IO ()
validateContent :: Content -> Resource -> IO (Either ContentError ())
```

### 4. Cache Management

```haskell
-- Cache types
data ResourceCache = ResourceCache
    { entries :: !(Map ResourceUri CacheEntry)
    , size :: !Integer
    , lastPurge :: !UTCTime
    } deriving (Eq, Show)

data CacheEntry = CacheEntry
    { content :: !Content
    , metadata :: !CacheMetadata
    , expires :: !UTCTime
    } deriving (Eq, Show)

-- Cache functions
getCached :: ResourceUri -> ResourceCache -> Maybe CacheEntry
updateCache :: ResourceUri -> Content -> ResourceCache -> ResourceCache
purgeExpired :: UTCTime -> ResourceCache -> ResourceCache
clearCache :: ResourceCache -> ResourceCache
```

## Testing Requirements

1. Unit Tests:
   - Resource discovery
   - Content retrieval
   - Cache management
   - Error handling

2. Integration Tests:
   - Server interaction
   - Content streaming
   - Cache behavior

3. Property Tests:
   - URI handling
   - Content validation
   - Cache properties

## Files to Create/Modify
1. `src/MCP/Client/Resource/Reader.hs` - Resource reader
2. `src/MCP/Client/Resource/Cache.hs` - Cache management
3. `src/MCP/Client/Resource/Discovery.hs` - Resource discovery
4. `test/MCP/Client/Resource/ReaderSpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - containers
  - time
  - bytestring
```

## Acceptance Criteria
1. Resource reading working
2. Discovery functional
3. Content retrieval complete
4. Cache management working
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Content transformation
2. Advanced caching
3. Content indexing
4. Content compression

## Resources
1. MCP Resource Specification
2. Content Handling Patterns
3. Cache Strategy Examples