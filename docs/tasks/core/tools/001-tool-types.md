# Task: Implement Core Tool Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core error types (001-error-types)

## Context
We need to implement the core types for the MCP tool system, which enables LLMs to execute predefined actions through MCP servers. These types need to support tool registration, input validation, and execution results.

## Requirements
1. Define core tool types
2. Implement input schema types
3. Create result handling types
4. Support tool validation
5. Enable tool registration

## Detailed Implementation Plan

### 1. Core Tool Types

```haskell
-- File: src/MCP/Core/Tool/Types.hs

-- Core tool type
data Tool = Tool
    { toolName :: !ToolName
    , description :: !(Maybe Text)
    , inputSchema :: !InputSchema
    , executionConfig :: !ExecutionConfig
    , metadata :: !ToolMetadata
    } deriving (Eq, Show)

-- Tool identification
newtype ToolName = ToolName 
    { unToolName :: Text }
    deriving (Eq, Show, IsString)

-- Tool metadata
data ToolMetadata = ToolMetadata
    { category :: !(Maybe Text)
    , version :: !(Maybe Text)
    , tags :: ![Text]
    , author :: !(Maybe Text)
    , deprecated :: !Bool
    } deriving (Eq, Show)

-- Execution configuration
data ExecutionConfig = ExecutionConfig
    { timeout :: !NominalDiffTime
    , maxRetries :: !Int
    , concurrencyLimit :: !(Maybe Int)
    , resourceLimits :: !ResourceLimits
    } deriving (Eq, Show)
```

### 2. Input Schema Types

```haskell
-- Input schema definition
data InputSchema = InputSchema
    { parameters :: ![Parameter]
    , required :: ![Text]  -- Required parameter names
    , additionalAllowed :: !Bool
    } deriving (Eq, Show)

data Parameter = Parameter
    { paramName :: !Text
    , paramType :: !ParameterType
    , description :: !(Maybe Text)
    , defaultValue :: !(Maybe Value)
    , constraints :: ![Constraint]
    } deriving (Eq, Show)

data ParameterType
    = StringType !StringFormat
    | NumberType !NumberFormat
    | BooleanType
    | ArrayType !ParameterType
    | ObjectType !InputSchema
    | EnumType ![Text]
    deriving (Eq, Show)

data Constraint
    = RangeConstraint !Value !Value  -- min, max
    | LengthConstraint !Int !Int     -- min, max
    | PatternConstraint !Text        -- regex
    | EnumConstraint ![Value]
    | CustomConstraint !Text !Value
    deriving (Eq, Show)
```

### 3. Result Handling

```haskell
-- Tool execution results
data ToolResult
    = SuccessResult !ExecutionResult
    | ErrorResult !ExecutionError
    | ProgressResult !ExecutionProgress
    deriving (Eq, Show)

data ExecutionResult = ExecutionResult
    { resultValue :: !Value
    , resultType :: !ResultType
    , metadata :: !ResultMetadata
    } deriving (Eq, Show)

data ResultType
    = TextResult
    | JsonResult
    | BinaryResult
    | VoidResult
    deriving (Eq, Show)

data ExecutionProgress = ExecutionProgress
    { completed :: !Int
    , total :: !Int
    , status :: !Text
    , timestamp :: !UTCTime
    } deriving (Eq, Show)
```

### 4. Tool Registration

```haskell
-- Tool registration types
data ToolRegistration = ToolRegistration
    { tool :: !Tool
    , handler :: !ToolHandler
    , validator :: !ToolValidator
    } deriving (Eq)

type ToolHandler = Value -> IO ToolResult
type ToolValidator = Value -> Either ToolError ()

-- Registration management
registerTool :: ToolRegistration -> IO ()
unregisterTool :: ToolName -> IO ()
lookupTool :: ToolName -> IO (Maybe ToolRegistration)
```

## Testing Requirements

1. Property Tests:
   - Schema validation
   - Input handling
   - Result processing

2. Unit Tests:
   - All tool types
   - Parameter validation
   - Registration functions

3. Integration Tests:
   - Tool execution
   - Error handling
   - Progress tracking

## Files to Create/Modify
1. `src/MCP/Core/Tool/Types.hs` - Core tool types
2. `src/MCP/Core/Tool/Schema.hs` - Schema handling
3. `src/MCP/Core/Tool/Registration.hs` - Registration system
4. `test/MCP/Core/Tool/TypesSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - aeson
  - time
  - text
  - containers
```

## Acceptance Criteria
1. Tool types implemented
2. Schema validation working
3. Registration system functional
4. Result handling complete
5. Progress tracking works
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Tool implementation logic
2. Complex scheduling
3. Tool composition
4. State management

## Resources
1. MCP Tool Specification
2. JSON Schema Standards
3. Tool Design Patterns