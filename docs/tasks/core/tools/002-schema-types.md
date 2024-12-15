# Task: Implement Tool Input Schema Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core tool types (001-tool-types)

## Context
We need to implement a type-safe JSON Schema implementation specifically for tool input validation in the MCP protocol. This system needs to support schema validation, type inference, and custom constraints while maintaining good error messages.

## Requirements
1. Define JSON Schema types
2. Implement schema validation
3. Add constraint handling
4. Support schema composition
5. Enable schema inference

## Detailed Implementation Plan

### 1. Schema Type System

```haskell
-- File: src/MCP/Core/Tool/Schema.hs

-- Core schema type
data Schema
    = StringSchema !StringSchema
    | NumberSchema !NumberSchema
    | IntegerSchema !IntegerSchema
    | BooleanSchema !BooleanSchema
    | ArraySchema !ArraySchema
    | ObjectSchema !ObjectSchema
    | UnionSchema ![Schema]
    | RefSchema !SchemaRef
    deriving (Eq, Show)

-- String schema
data StringSchema = StringSchema
    { minLength :: !(Maybe Int)
    , maxLength :: !(Maybe Int)
    , pattern :: !(Maybe Text)
    , format :: !(Maybe StringFormat)
    , enumValues :: !(Maybe [Text])
    } deriving (Eq, Show)

-- Number schema
data NumberSchema = NumberSchema
    { minimum :: !(Maybe Scientific)
    , maximum :: !(Maybe Scientific)
    , exclusiveMinimum :: !(Maybe Scientific)
    , exclusiveMaximum :: !(Maybe Scientific)
    , multipleOf :: !(Maybe Scientific)
    } deriving (Eq, Show)
```

### 2. Object and Array Schemas

```haskell
-- Object schema
data ObjectSchema = ObjectSchema
    { properties :: !(Map Text Schema)
    , required :: ![Text]
    , additionalProperties :: !AdditionalProperties
    , minProperties :: !(Maybe Int)
    , maxProperties :: !(Maybe Int)
    , propertyNames :: !(Maybe StringSchema)
    } deriving (Eq, Show)

data AdditionalProperties
    = NoAdditional
    | AllowAdditional
    | SchemaAdditional !Schema
    deriving (Eq, Show)

-- Array schema
data ArraySchema = ArraySchema
    { items :: !Items
    , minItems :: !(Maybe Int)
    , maxItems :: !(Maybe Int)
    , uniqueItems :: !Bool
    , contains :: !(Maybe Schema)
    } deriving (Eq, Show)

data Items
    = SingleItems !Schema
    | TupleItems ![Schema]
    deriving (Eq, Show)
```

### 3. Schema References

```haskell
-- Schema references and definitions
newtype SchemaRef = SchemaRef 
    { unSchemaRef :: Text }
    deriving (Eq, Show)

data SchemaDefinitions = SchemaDefinitions
    { definitions :: !(Map Text Schema)
    , root :: !Schema
    } deriving (Eq, Show)

-- Reference resolution
resolveRef :: SchemaRef -> SchemaDefinitions -> Either SchemaError Schema
addDefinition :: Text -> Schema -> SchemaDefinitions -> SchemaDefinitions
```

### 4. Validation System

```haskell
-- Validation types
data ValidationContext = ValidationContext
    { path :: ![Text]
    , definitions :: !SchemaDefinitions
    , options :: !ValidationOptions
    } deriving (Eq, Show)

data ValidationOptions = ValidationOptions
    { formatValidation :: !Bool
    , maxErrors :: !(Maybe Int)
    , strictNumbers :: !Bool
    } deriving (Eq, Show)

-- Validation functions
validateValue :: Schema -> Value -> Either [ValidationError] ()
validateObject :: ObjectSchema -> Object -> ValidationContext -> Either [ValidationError] ()
validateArray :: ArraySchema -> Array -> ValidationContext -> Either [ValidationError] ()
```

## Testing Requirements

1. Property Tests:
   - Schema validation
   - Reference resolution
   - Type inference

2. Unit Tests:
   - All schema types
   - Validation rules
   - Error messages

3. Integration Tests:
   - With tool system
   - Complex schemas
   - Performance testing

## Files to Create/Modify
1. `src/MCP/Core/Tool/Schema.hs` - Core schema types
2. `src/MCP/Core/Tool/Validation.hs` - Validation logic
3. `src/MCP/Core/Tool/Reference.hs` - Reference handling
4. `test/MCP/Core/Tool/SchemaSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - scientific
  - text
  - containers
```

## Acceptance Criteria
1. Schema types implemented
2. Validation system working
3. Reference handling functional
4. Constraint checking complete
5. Error messages clear
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Full JSON Schema support
2. Schema generation
3. Schema optimization
4. Remote references

## Resources
1. JSON Schema Specification
2. Tool Schema Requirements
3. Validation Best Practices