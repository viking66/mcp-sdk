# Task: Implement Resource Metadata Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core resource types (001-resource-types)
- Resource content types (002-content-types)

## Context
We need to implement a flexible and extensible metadata system for MCP resources. This system should handle standard metadata fields while allowing for custom metadata, versioning information, and content-type specific metadata.

## Requirements
1. Define core metadata types
2. Support standard and custom fields
3. Implement metadata validation
4. Add serialization support
5. Enable metadata querying

## Detailed Implementation Plan

### 1. Core Metadata Types

```haskell
-- File: src/MCP/Core/Resource/Metadata.hs

-- Core metadata type
data Metadata = Metadata
    { standardFields :: !StandardMetadata
    , customFields :: !CustomMetadata
    , contentMetadata :: !ContentMetadata
    , systemMetadata :: !SystemMetadata
    } deriving (Eq, Show)

-- Standard metadata fields
data StandardMetadata = StandardMetadata
    { created :: !UTCTime
    , modified :: !UTCTime
    , size :: !(Maybe Integer)
    , checksum :: !(Maybe Checksum)
    , version :: !(Maybe Version)
    } deriving (Eq, Show)

-- Custom metadata fields
newtype CustomMetadata = CustomMetadata
    { unCustom :: Map Text Value }
    deriving (Eq, Show, Semigroup, Monoid)

-- Content-type specific metadata
data ContentMetadata = ContentMetadata
    { encoding :: !(Maybe Text)
    , language :: !(Maybe Text)
    , compression :: !(Maybe Text)
    , disposition :: !(Maybe Text)
    } deriving (Eq, Show)

-- System metadata
data SystemMetadata = SystemMetadata
    { permissions :: !(Maybe Permissions)
    , owner :: !(Maybe Text)
    , tags :: ![Text]
    , hidden :: !Bool
    } deriving (Eq, Show)
```

### 2. Metadata Operations

```haskell
-- Metadata creation and manipulation
mkMetadata :: UTCTime -> IO Metadata
updateMetadata :: Metadata -> IO Metadata
mergeMetadata :: Metadata -> Metadata -> Metadata

-- Field access
getStandardField :: StandardField -> Metadata -> Maybe Value
getCustomField :: Text -> Metadata -> Maybe Value
setCustomField :: Text -> Value -> Metadata -> Metadata

-- Validation
validateMetadata :: Metadata -> Either MetadataError ()
validateCustomField :: Text -> Value -> Either MetadataError ()
validateContentMetadata :: ContentMetadata -> Either MetadataError ()
```

### 3. Query Support

```haskell
-- Query types
data MetadataQuery
    = FieldEquals !FieldPath !Value
    | FieldContains !FieldPath !Value
    | FieldExists !FieldPath
    | And ![MetadataQuery]
    | Or ![MetadataQuery]
    | Not !MetadataQuery
    deriving (Eq, Show)

newtype FieldPath = FieldPath
    { unFieldPath :: [Text] }
    deriving (Eq, Show)

-- Query execution
queryMetadata :: MetadataQuery -> Metadata -> Bool
findFields :: FieldPath -> Metadata -> [Value]
```

### 4. Serialization

```haskell
-- Serialization support
class MetadataFormat f where
    serializeMetadata :: Metadata -> f
    deserializeMetadata :: f -> Either MetadataError Metadata

-- JSON instance
instance MetadataFormat Value where
    serializeMetadata = toJSON
    deserializeMetadata = parseJSON

-- Custom format support
data MetadataFormat
    = JsonFormat
    | XmlFormat
    | CustomFormat !Text
    deriving (Eq, Show)
```

## Testing Requirements

1. Property Tests:
   - Metadata operations
   - Query execution
   - Serialization

2. Unit Tests:
   - All metadata types
   - Field validation
   - Query parsing

3. Integration Tests:
   - With resource types
   - Format handling
   - Query performance

## Files to Create/Modify
1. `src/MCP/Core/Resource/Metadata.hs` - Core metadata types
2. `src/MCP/Core/Resource/Query.hs` - Query support
3. `src/MCP/Core/Resource/Format.hs` - Format handling
4. `test/MCP/Core/Resource/MetadataSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - time
  - aeson
  - containers
  - text
```

## Acceptance Criteria
1. Metadata types implemented
2. Query system working
3. Validation complete
4. Serialization functional
5. Custom fields supported
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex query optimization
2. Metadata indexing
3. Metadata versioning
4. Access control

## Resources
1. Resource Metadata Standards
2. Query Language Design
3. JSON Schema Standards