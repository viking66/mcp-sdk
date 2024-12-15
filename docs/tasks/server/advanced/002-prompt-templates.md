# Task: Implement Prompt Templates

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: mcp-server
**Prerequisites**: 
- Core prompt types (mcp-core/prompts/001-prompt-types)
- Protocol capability types (mcp-core/protocol/003-capability-types)

## Context
We need to implement support for prompt templates in the MCP server. This includes template definition, variable substitution, validation, and rendering. The system should support both simple and complex templates while maintaining type safety and good error handling.

## Requirements
1. Define template types
2. Implement template parsing
3. Create template rendering
4. Support template validation
5. Enable template management

## Detailed Implementation Plan

### 1. Template Types

```haskell
-- File: src/MCP/Server/Prompt/Template.hs

-- Core template types
data PromptTemplate = PromptTemplate
    { templateId :: !TemplateId
    , name :: !Text
    , description :: !(Maybe Text)
    , parts :: ![TemplatePart]
    , variables :: ![TemplateVariable]
    } deriving (Eq, Show)

data TemplatePart
    = TextPart !Text
    | VariablePart !VariableRef
    | ConditionalPart !Condition !TemplatePart !TemplatePart
    | RepeatPart !VariableRef !TemplatePart !Separator
    deriving (Eq, Show)

data TemplateVariable = TemplateVariable
    { name :: !Text
    , varType :: !VariableType
    , required :: !Bool
    , defaultValue :: !(Maybe Value)
    , validation :: ![Constraint]
    } deriving (Eq, Show)
```

### 2. Template Parsing

```haskell
-- Parser types
data TemplateParser = TemplateParser
    { config :: !ParserConfig
    , state :: !ParserState
    , context :: !ParsingContext
    } deriving (Eq)

data ParserConfig = ParserConfig
    { maxNesting :: !Int
    , allowedFunctions :: ![Text]
    , strictMode :: !Bool
    } deriving (Eq, Show)

-- Parsing functions
parseTemplate :: Text -> Either ParseError PromptTemplate
parseTemplatePart :: Text -> Either ParseError TemplatePart
validateTemplate :: PromptTemplate -> Either ValidationError ()
```

### 3. Template Rendering

```haskell
-- Rendering types
data RenderContext = RenderContext
    { variables :: !(Map Text Value)
    , functions :: !(Map Text Function)
    , options :: !RenderOptions
    } deriving (Eq)

data RenderOptions = RenderOptions
    { escaping :: !EscapeMode
    , formatting :: !FormatOptions
    , maxLength :: !(Maybe Int)
    } deriving (Eq, Show)

-- Rendering functions
renderTemplate :: PromptTemplate -> RenderContext -> Either RenderError Text
renderPart :: TemplatePart -> RenderContext -> Either RenderError Text
applyFormatting :: Text -> FormatOptions -> Text
```

### 4. Template Management

```haskell
-- Management types
data TemplateManager = TemplateManager
    { templates :: !(TVar (Map TemplateId PromptTemplate))
    , config :: !ManagerConfig
    , metrics :: !(TVar TemplateMetrics)
    } deriving (Eq)

data ManagerConfig = ManagerConfig
    { maxTemplates :: !Int
    , cacheDuration :: !NominalDiffTime
    , validateOnAdd :: !Bool
    } deriving (Eq, Show)

-- Management functions
addTemplate :: PromptTemplate -> TemplateManager -> IO (Either TemplateError ())
removeTemplate :: TemplateId -> TemplateManager -> IO ()
updateTemplate :: TemplateId -> PromptTemplate -> TemplateManager -> IO (Either TemplateError ())
```

## Testing Requirements

1. Unit Tests:
   - Template parsing
   - Variable handling
   - Rendering logic
   - Error cases

2. Integration Tests:
   - With prompt system
   - Template management
   - Error handling

3. Property Tests:
   - Parser properties
   - Renderer properties
   - Variable handling

## Files to Create/Modify
1. `src/MCP/Server/Prompt/Template.hs` - Template types
2. `src/MCP/Server/Prompt/Parser.hs` - Template parsing
3. `src/MCP/Server/Prompt/Render.hs` - Template rendering
4. `test/MCP/Server/Prompt/TemplateSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - parsec
  - text
  - aeson
  - mtl
```

## Acceptance Criteria
1. Template parsing working
2. Rendering functional
3. Variables handled correctly
4. Management complete
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex template logic
2. Template versioning
3. Template sharing
4. Template discovery

## Resources
1. Template Engine Patterns
2. Parser Implementation Guide
3. Rendering Strategies