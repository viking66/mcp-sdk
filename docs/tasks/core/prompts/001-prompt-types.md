# Task: Implement Core Prompt Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: None

## Context
We need to implement the core types for the MCP prompt system, which enables servers to expose reusable prompt templates to clients. These types need to support template parameters, argument validation, and prompt generation.

## Requirements
1. Define core prompt types
2. Implement template system
3. Create argument handling
4. Support prompt validation
5. Enable prompt generation

## Detailed Implementation Plan

### 1. Core Prompt Types

```haskell
-- File: src/MCP/Core/Prompt/Types.hs

-- Core prompt type
data Prompt = Prompt
    { promptName :: !PromptName
    , description :: !(Maybe Text)
    , arguments :: ![PromptArgument]
    , template :: !PromptTemplate
    , metadata :: !PromptMetadata
    } deriving (Eq, Show)

-- Prompt identification
newtype PromptName = PromptName 
    { unPromptName :: Text }
    deriving (Eq, Show, IsString)

-- Prompt metadata
data PromptMetadata = PromptMetadata
    { category :: !(Maybe Text)
    , tags :: ![Text]
    , version :: !(Maybe Text)
    , author :: !(Maybe Text)
    , examples :: ![PromptExample]
    } deriving (Eq, Show)

-- Prompt example
data PromptExample = PromptExample
    { exampleName :: !Text
    , arguments :: !(Map Text Value)
    , expectedOutput :: !(Maybe Text)
    , description :: !(Maybe Text)
    } deriving (Eq, Show)
```

### 2. Template System

```haskell
-- Template types
data PromptTemplate
    = TextTemplate !Text
    | MultipartTemplate ![TemplatePart]
    | FunctionTemplate !TemplateFunction
    deriving (Eq, Show)

data TemplatePart
    = TextPart !Text
    | ArgumentPart !Text
    | ConditionalPart !Condition !TemplatePart !TemplatePart
    | RepeatingPart !Text !TemplatePart
    deriving (Eq, Show)

data Condition
    = HasArgument !Text
    | NotEmpty !Text
    | Equals !Text !Value
    deriving (Eq, Show)

type TemplateFunction = Map Text Value -> Either PromptError Text
```

### 3. Argument Handling

```haskell
-- Argument types
data PromptArgument = PromptArgument
    { argName :: !Text
    , argType :: !ArgumentType
    , description :: !(Maybe Text)
    , required :: !Bool
    , defaultValue :: !(Maybe Value)
    , validation :: ![ArgumentConstraint]
    } deriving (Eq, Show)

data ArgumentType
    = StringArg !StringFormat
    | NumberArg !NumberFormat
    | BooleanArg
    | ListArg !ArgumentType
    | EnumArg ![Text]
    deriving (Eq, Show)

data ArgumentConstraint
    = MinLength !Int
    | MaxLength !Int
    | Pattern !Text
    | Range !Scientific !Scientific
    | Custom !Text !(Value -> Bool)
    deriving (Eq)
```

### 4. Generation System

```haskell
-- Generation types
data GeneratedPrompt = GeneratedPrompt
    { prompt :: !Text
    , metadata :: !GenerationMetadata
    , source :: !Prompt
    } deriving (Eq, Show)

data GenerationMetadata = GenerationMetadata
    { arguments :: !(Map Text Value)
    , timestamp :: !UTCTime
    , renderedParts :: ![Text]
    } deriving (Eq, Show)

-- Generation functions
generatePrompt :: Prompt -> Map Text Value -> Either PromptError GeneratedPrompt
validateArguments :: Prompt -> Map Text Value -> Either PromptError ()
resolveTemplate :: PromptTemplate -> Map Text Value -> Either PromptError Text
```

## Testing Requirements

1. Property Tests:
   - Template generation
   - Argument validation
   - Condition evaluation

2. Unit Tests:
   - All prompt types
   - Template parts
   - Generation process

3. Integration Tests:
   - Complex templates
   - Argument handling
   - Error scenarios

## Files to Create/Modify
1. `src/MCP/Core/Prompt/Types.hs` - Core prompt types
2. `src/MCP/Core/Prompt/Template.hs` - Template system
3. `src/MCP/Core/Prompt/Generation.hs` - Generation logic
4. `test/MCP/Core/Prompt/TypesSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - text
  - aeson
  - time
  - containers
```

## Acceptance Criteria
1. Prompt types implemented
2. Template system working
3. Argument handling functional
4. Generation system complete
5. Validation working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex template logic
2. Template optimization
3. Prompt versioning
4. Template sharing

## Resources
1. MCP Prompt Specification
2. Template Design Patterns
3. Argument Validation Examples