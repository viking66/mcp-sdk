# Task: Implement Prompt Template Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core prompt types (001-prompt-types)

## Context
We need to implement a flexible and type-safe template system for MCP prompts that supports variable substitution, conditions, iterations, and custom formatting. The system should be easy to use while maintaining robustness and good error handling.

## Requirements
1. Define template syntax types
2. Implement template parsing
3. Add template compilation
4. Support template validation
5. Enable template composition

## Detailed Implementation Plan

### 1. Template Syntax

```haskell
-- File: src/MCP/Core/Prompt/Template/Syntax.hs

-- Core syntax types
data TemplateExpr
    = Literal !Text
    | Variable !VarExpr
    | Conditional !CondExpr
    | Loop !LoopExpr
    | Format !FormatExpr
    deriving (Eq, Show)

-- Variable expressions
data VarExpr = VarExpr
    { varName :: !Text
    , modifier :: !(Maybe Modifier)
    , fallback :: !(Maybe TemplateExpr)
    } deriving (Eq, Show)

-- Conditional expressions
data CondExpr = CondExpr
    { condition :: !Condition
    , thenBranch :: !TemplateExpr
    , elseBranch :: !(Maybe TemplateExpr)
    } deriving (Eq, Show)

-- Loop expressions
data LoopExpr = LoopExpr
    { items :: !Text
    , itemVar :: !Text
    , indexVar :: !(Maybe Text)
    , body :: !TemplateExpr
    , separator :: !(Maybe Text)
    } deriving (Eq, Show)
```

### 2. Modifiers and Formatting

```haskell
-- Modifier types
data Modifier
    = Case !CaseModifier
    | Trim !TrimModifier
    | Wrap !WrapModifier
    | Custom !Text
    deriving (Eq, Show)

data CaseModifier
    = Uppercase
    | Lowercase
    | Titlecase
    | Sentencecase
    deriving (Eq, Show)

data TrimModifier
    = TrimBoth
    | TrimLeft
    | TrimRight
    | TrimLines
    deriving (Eq, Show)

-- Format expressions
data FormatExpr = FormatExpr
    { template :: !TemplateExpr
    , format :: !Format
    } deriving (Eq, Show)

data Format
    = Indent !Int
    | Align !Alignment
    | Width !Int
    | Custom !Text !Value
    deriving (Eq, Show)
```

### 3. Template Compilation

```haskell
-- Compilation types
data CompiledTemplate = CompiledTemplate
    { expr :: !TemplateExpr
    , variables :: ![Text]
    , conditions :: ![Condition]
    } deriving (Show)

-- Compilation functions
compileTemplate :: Text -> Either TemplateError CompiledTemplate
optimizeTemplate :: CompiledTemplate -> CompiledTemplate
validateTemplate :: CompiledTemplate -> Either TemplateError ()

-- Template context
data TemplateContext = TemplateContext
    { variables :: !(Map Text Value)
    , modifiers :: !(Map Text ModifierFn)
    , formats :: !(Map Text FormatFn)
    }

type ModifierFn = Text -> Either TemplateError Text
type FormatFn = Text -> Value -> Either TemplateError Text
```

### 4. Template Rendering

```haskell
-- Rendering types
data RenderOptions = RenderOptions
    { maxDepth :: !Int
    , timeout :: !NominalDiffTime
    , strictMode :: !Bool
    } deriving (Show)

-- Rendering functions
renderTemplate :: CompiledTemplate -> TemplateContext -> Either TemplateError Text
renderWithOptions :: RenderOptions -> CompiledTemplate -> TemplateContext -> Either TemplateError Text

-- Result types
data RenderResult = RenderResult
    { output :: !Text
    , usedVars :: ![Text]
    , stats :: !RenderStats
    } deriving (Show)

data RenderStats = RenderStats
    { renderTime :: !NominalDiffTime
    , nodeCount :: !Int
    , depth :: !Int
    } deriving (Show)
```

## Testing Requirements

1. Property Tests:
   - Template parsing
   - Variable handling
   - Condition evaluation

2. Unit Tests:
   - All syntax nodes
   - Modifiers
   - Formatting

3. Integration Tests:
   - Complex templates
   - Error handling
   - Performance testing

## Files to Create/Modify
1. `src/MCP/Core/Prompt/Template/Syntax.hs` - Core syntax types
2. `src/MCP/Core/Prompt/Template/Compiler.hs` - Compilation logic
3. `src/MCP/Core/Prompt/Template/Renderer.hs` - Rendering engine
4. `test/MCP/Core/Prompt/Template/SyntaxSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - text
  - containers
  - time
  - parsec
```

## Acceptance Criteria
1. Template syntax implemented
2. Compilation working
3. Rendering functional
4. Modifiers implemented
5. Error handling complete
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex template logic
2. Template optimization
3. Custom language features
4. Runtime compilation

## Resources
1. Template Engine Design Patterns
2. Text Formatting Best Practices
3. Error Handling Examples