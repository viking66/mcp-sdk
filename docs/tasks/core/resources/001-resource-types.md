# Task: Implement Core Resource Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error types (001-error-types)

## Context
We need to implement the core resource types used in the MCP protocol. Resources represent data or content that can be accessed by clients, such as files, API responses, or live system data. These types need to support different content types, metadata, and serialization formats.

## Requirements
1. Define core resource types
2. Implement resource identification
3. Support resource metadata
4. Enable content type handling
5. Provide serialization support

## Detailed Implementation Plan

### 1. Core Resource Types

```haskell
-- File: src/MCP/Core/Resource/Types.hs

-- Core resource type
data Resource = Resource
    { resourceUri :: !ResourceUri
    , resourceName :: !Text
    , resourceDescription :: !(Maybe Text)
    , resourceMimeType :: !(Maybe MimeType)
    , resourceMetadata :: !ResourceMetadata
    } deriving (Eq, Show)

-- Resource URI handling
newtype ResourceUri = ResourceUri
    { unResourceUri :: Text }
    deriving (Eq, Show, IsString)

-- Resource content
data ResourceContent
    = TextContent !Text
    | BinaryContent !ByteString
    | JsonContent !Value
    | NoContent
    deriving (Eq, Show)

-- MIME type handling
data MimeType = MimeType
    { mimeType :: !Text
    , mimeSubtype :: !Text
    , mimeParameters :: !(Map Text Text)
    } deriving (Eq, Show)

-- Resource metadata
data ResourceMetadata = ResourceMetadata
    { contentLength :: !(Maybe Int64)
    , lastModified :: !(Maybe UTCTime)
    , etag :: !(Maybe Text)
    , customMetadata :: !(Map Text Value)
    } deriving (Eq, Show)
```

### 2. Resource Operations

```haskell
-- Resource creation and validation
mkResource :: ResourceUri -> Text -> IO Resource
validateResource :: Resource -> Either ResourceError ()
validateResourceUri :: ResourceUri -> Either ResourceError ()

-- Content handling
getResourceContent :: Resource -> IO ResourceContent
setResourceContent :: ResourceContent -> Resource -> IO Resource
detectMimeType :: ResourceContent -> Maybe MimeType

-- Metadata operations
updateMetadata :: ResourceMetadata -> Resource -> Resource
addMetadata :: Text -> Value -> Resource -> Resource
getMetadataValue :: Text -> Resource -> Maybe Value
```

### 3. Resource Templates

```haskell
data ResourceTemplate = ResourceTemplate
    { templatePattern :: !Text
    , templateParameters :: ![TemplateParameter]
    , templateDescription :: !(Maybe Text)
    } deriving (Eq, Show)

data TemplateParameter = TemplateParameter
    { paramName :: !Text
    , paramType :: !ParameterType
    , paramDescription :: !(Maybe Text)
    , paramRequired :: !Bool
    } deriving (Eq, Show)

data ParameterType
    = StringParam
    | IntegerParam
    | BooleanParam
    | EnumParam ![Text]
    deriving (Eq, Show)
```

### 4. URI Handling

```haskell
-- URI manipulation and validation
parseResourceUri :: Text -> Either ResourceError ResourceUri
formatResourceUri :: ResourceUri -> Text
isValidResourceUri :: ResourceUri -> Bool

-- URI template handling
expandTemplate :: ResourceTemplate -> Map Text Text -> Either ResourceError ResourceUri
matchTemplate :: ResourceTemplate -> ResourceUri -> Maybe (Map Text Text)
```

## Testing Requirements

1. Property Tests:
   - Resource creation/validation
   - URI handling
   - Template expansion

2. Unit Tests:
   - All resource operations
   - MIME type handling
   - Metadata management

3. Integration Tests:
   - Template matching
   - Content handling
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Core/Resource/Types.hs` - Core resource types
2. `src/MCP/Core/Resource/Uri.hs` - URI handling
3. `src/MCP/Core/Resource/Template.hs` - Template logic
4. `test/MCP/Core/Resource/TypesSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - text
  - bytestring
  - aeson
  - containers
  - time
```

## Acceptance Criteria
1. Resource types implemented
2. URI handling working
3. Templates functional
4. Content handling complete
5. Metadata support working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Implementing resource storage
2. Building content transformations
3. Access control logic
4. Resource versioning

## Resources
1. MCP Resource Specification
2. URI Template RFC
3. MIME Type Standards