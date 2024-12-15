# Task: Implement Project Templates

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Low (P3)  
**Package**: mcp-core
**Prerequisites**: All core functionality implemented

## Context
We need to create project templates to help developers quickly start new MCP projects. These templates should cover common use cases, provide proper configuration, and include necessary boilerplate while being easy to customize.

## Requirements
1. Create template engine
2. Implement core templates
3. Add customization support
4. Support template validation
5. Enable template generation

## Detailed Implementation Plan

### 1. Template Engine

```haskell
-- File: src/MCP/Template/Engine.hs

-- Template engine types
data TemplateEngine = TemplateEngine
    { templates :: !(Map TemplateName Template)
    , generators :: !(Map GeneratorName Generator)
    , config :: !EngineConfig
    } deriving (Eq)

data Template = Template
    { name :: !TemplateName
    , files :: ![TemplateFile]
    , variables :: ![TemplateVariable]
    , hooks :: !TemplateHooks
    } deriving (Eq, Show)

-- Template functions
generateProject :: Template -> ProjectConfig -> IO (Either TemplateError ())
customizeTemplate :: Template -> CustomConfig -> Template
validateTemplate :: Template -> Either ValidationError ()
```

### 2. Core Templates

```haskell
-- File: templates/basic-server.yaml
name: basic-server
description: Basic MCP server implementation
variables:
  - name: projectName
    type: string
    default: mcp-server
  - name: author
    type: string
  - name: version
    type: string
    default: "0.1.0.0"

files:
  - path: "{{projectName}}.cabal"
    template: |
      cabal-version: 3.0
      name: {{projectName}}
      version: {{version}}
      author: {{author}}
      
      library
          exposed-modules: MCP.Server
          build-depends: mcp-core, mcp-server
          hs-source-dirs: src
          default-language: GHC2021

  - path: "src/MCP/Server.hs"
    template: |
      module MCP.Server where
      
      import MCP.Core
      import MCP.Server.Core
      
      main :: IO ()
      main = do
          server <- initServer defaultConfig
          runServer server
```

### 3. Template Variables

```haskell
-- File: src/MCP/Template/Variables.hs

-- Variable types
data TemplateVariable = TemplateVariable
    { name :: !Text
    , varType :: !VarType
    , default_ :: !(Maybe Value)
    , validator :: !(Maybe Validator)
    } deriving (Eq, Show)

data VarType
    = StringType
    | BoolType
    | NumberType
    | ListType !VarType
    | CustomType !Text
    deriving (Eq, Show)

-- Variable functions
resolveVariables :: Template -> Map Text Value -> Either VarError Template
validateVariables :: Template -> Map Text Value -> Either VarError ()
defaultVariables :: Template -> Map Text Value
```

### 4. Project Generation

```haskell
-- File: src/MCP/Template/Generator.hs

-- Generator types
data ProjectGenerator = ProjectGenerator
    { config :: !GeneratorConfig
    , hooks :: !GeneratorHooks
    , validation :: !ValidationConfig
    } deriving (Eq)

data GeneratorConfig = GeneratorConfig
    { targetDir :: !FilePath
    , overwrite :: !Bool
    , gitInit :: !Bool
    } deriving (Eq, Show)

-- Generation functions
generateFiles :: Template -> GeneratorConfig -> IO (Either GenError ())
runHooks :: Template -> [Hook] -> IO (Either HookError ())
cleanupOnFailure :: GeneratorConfig -> IO ()
```

## Testing Requirements

1. Unit Tests:
   - Template engine
   - Variable resolution
   - File generation
   - Validation

2. Integration Tests:
   - Full project generation
   - Template customization
   - Hook execution

3. Template Tests:
   - All core templates
   - Variable combinations
   - Error conditions

## Files to Create/Modify
1. `src/MCP/Template/Engine.hs` - Template engine
2. `src/MCP/Template/Variables.hs` - Variable handling
3. `src/MCP/Template/Generator.hs` - Project generation
4. `templates/` - Core templates
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - mustache
  - filepath
  - directory
  - yaml
```

## Acceptance Criteria
1. Template engine working
2. Core templates complete
3. Generation functional
4. Variables working
5. Hooks executing
6. Documentation clear
7. Easy to use
8. Code passes checks

## Non-Goals
1. Complex templates
2. Build system
3. Deployment config
4. IDE integration

## Resources
1. Project Template Patterns
2. Generation Tools Examples
3. Template Engine Guides