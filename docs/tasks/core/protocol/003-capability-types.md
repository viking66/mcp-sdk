# Task: Implement Protocol Capability Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Protocol version types (001-version-types)
- Feature negotiation types (002-feature-types)

## Context
We need to implement types that define specific protocol capabilities for MCP. These types represent the actual functionality that clients and servers can support, such as resources, prompts, tools, and sampling. The implementation needs to support capability-specific configuration and validation.

## Requirements
1. Define protocol capability types
2. Implement capability configuration
3. Create validation system
4. Support capability metadata
5. Enable capability constraints

## Detailed Implementation Plan

### 1. Core Capability Types

```haskell
-- File: src/MCP/Core/Protocol/Capability.hs

-- Core protocol capabilities
data ProtocolCapability
    = ResourceCapability !ResourceConfig
    | PromptCapability !PromptConfig
    | ToolCapability !ToolConfig
    | SamplingCapability !SamplingConfig
    | CustomCapability !Text !Value
    deriving (Eq, Show)

-- Resource capability
data ResourceConfig = ResourceConfig
    { supportsSubscriptions :: !Bool
    , supportedTypes :: ![MimeType]
    , maxSize :: !(Maybe Integer)
    , concurrent :: !Bool
    } deriving (Eq, Show)

-- Prompt capability
data PromptConfig = PromptConfig
    { supportsTemplates :: !Bool
    , maxPromptLength :: !(Maybe Int)
    , supportedFormats :: ![PromptFormat]
    } deriving (Eq, Show)

-- Tool capability
data ToolConfig = ToolConfig
    { supportsAsync :: !Bool
    , maxConcurrent :: !(Maybe Int)
    , supportedSchemas :: ![SchemaFormat]
    } deriving (Eq, Show)

-- Sampling capability
data SamplingConfig = SamplingConfig
    { supportedModels :: ![Text]
    , maxTokens :: !(Maybe Int)
    , supportsStreaming :: !Bool
    } deriving (Eq, Show)
```

### 2. Capability Configuration

```haskell
-- Configuration types
data CapabilityConfig = CapabilityConfig
    { enabled :: !Bool
    , settings :: !ConfigSettings
    , constraints :: ![Constraint]
    } deriving (Eq, Show)

data ConfigSettings = ConfigSettings
    { timeout :: !NominalDiffTime
    , retries :: !Int
    , errorHandling :: !ErrorHandling
    } deriving (Eq, Show)

-- Configuration validation
validateConfig :: CapabilityConfig -> Either ConfigError ()
mergeConfigs :: CapabilityConfig -> CapabilityConfig -> CapabilityConfig
defaultConfig :: ProtocolCapability -> CapabilityConfig
```

### 3. Capability Metadata

```haskell
-- Metadata types
data CapabilityInfo = CapabilityInfo
    { name :: !Text
    , description :: !(Maybe Text)
    , documentation :: !(Maybe Text)
    , examples :: ![Example]
    , stability :: !StabilityLevel
    } deriving (Eq, Show)

data Example = Example
    { title :: !Text
    , code :: !Text
    , explanation :: !(Maybe Text)
    } deriving (Eq, Show)

data StabilityLevel
    = Stable
    | Beta
    | Experimental
    | Deprecated
    deriving (Eq, Show)

-- Metadata utilities
getCapabilityInfo :: ProtocolCapability -> CapabilityInfo
updateMetadata :: CapabilityInfo -> ProtocolCapability -> ProtocolCapability
```

### 4. Capability Constraints

```haskell
-- Constraint types
data Constraint
    = ResourceConstraint !ResourceLimit
    | TimeConstraint !TimeLimit
    | RateConstraint !RateLimit
    | CustomConstraint !Text !Value
    deriving (Eq, Show)

data ResourceLimit = ResourceLimit
    { resource :: !Text
    , limit :: !Integer
    , window :: !(Maybe NominalDiffTime)
    } deriving (Eq, Show)

data RateLimit = RateLimit
    { maxRequests :: !Int
    , perInterval :: !NominalDiffTime
    } deriving (Eq, Show)

-- Constraint checking
checkConstraints :: [Constraint] -> CapabilityUse -> IO (Either ConstraintError ())
updateUsage :: CapabilityUse -> [Constraint] -> IO CapabilityUse
```

## Testing Requirements

1. Property Tests:
   - Configuration validation
   - Constraint checking
   - Metadata handling

2. Unit Tests:
   - All capability types
   - Configuration merging
   - Constraint evaluation

3. Integration Tests:
   - With feature system
   - Resource tracking
   - Error handling

## Files to Create/Modify
1. `src/MCP/Core/Protocol/Capability.hs` - Core capability types
2. `src/MCP/Core/Protocol/Config.hs` - Configuration handling
3. `src/MCP/Core/Protocol/Constraint.hs` - Constraint system
4. `test/MCP/Core/Protocol/CapabilitySpec.hs` - Tests
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
1. Capability types implemented
2. Configuration system working
3. Metadata handling functional
4. Constraints effective
5. Error handling complete
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Capability discovery
2. Dynamic reconfiguration
3. Complex scheduling
4. Usage analytics

## Resources
1. MCP Protocol Specification
2. Capability Configuration Patterns
3. Resource Constraint Examples