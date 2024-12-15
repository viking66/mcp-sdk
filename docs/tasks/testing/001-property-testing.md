# Task: Implement Property Testing Framework

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: High (P1)  
**Package**: mcp-core, mcp-server
**Prerequisites**: 
- All core types implemented
- Basic server functionality

## Context
We need to implement a comprehensive property testing framework for the MCP implementation. This framework should enable testing of invariants, round-trip properties, and state machine properties across all components of the system.

## Requirements
1. Define property test types
2. Implement generators
3. Create property combinators
4. Support state machine testing
5. Enable shrinking strategies

## Detailed Implementation Plan

### 1. Property Test Types

```haskell
-- File: src/MCP/Test/Property/Types.hs

-- Core property types
data Property a = Property
    { propertyName :: !Text
    , generator :: !Gen a
    , assertion :: !(a -> PropertyT IO ())
    , config :: !PropertyConfig
    } deriving (Functor)

-- Property configuration
data PropertyConfig = PropertyConfig
    { numTests :: !Int
    , maxDiscard :: !Int
    , maxSize :: !Int
    , maxShrinks :: !Int
    } deriving (Eq, Show)

-- Test categories
data PropertyCategory
    = ProtocolProperty
    | TransportProperty
    | ResourceProperty
    | ToolProperty
    | SessionProperty
    deriving (Eq, Show)
```

### 2. Test Generators

```haskell
-- Generator types
data GeneratorConfig = GeneratorConfig
    { sizeRange :: !(Int, Int)
    , allowEmpty :: !Bool
    , distribution :: !Distribution
    } deriving (Eq, Show)

-- Core generators
genVersion :: Gen Version
genVersion = do
    major <- chooseInt (0, 10)
    minor <- chooseInt (0, 100)
    patch <- chooseInt (0, 100)
    pure $ Version major minor patch

genCapability :: Gen ProtocolCapability
genCapability = frequency
    [ (3, genResourceCapability)
    , (3, genToolCapability)
    , (2, genPromptCapability)
    , (1, genSamplingCapability)
    ]

genMessage :: Gen Message
genMessage = oneof
    [ genRequestMessage
    , genResponseMessage
    , genNotification
    ]

genResource :: Gen Resource
genResource = do
    uri <- genResourceUri
    name <- genResourceName
    content <- genResourceContent
    pure $ Resource uri name content
```

### 3. Property Combinators

```haskell
-- Property composition
data PropertyComposition
    = Sequential ![Property a]
    | Parallel ![Property a]
    | Alternative ![Property a]
    deriving (Functor)

-- Combinator functions
(.&&.) :: Property a -> Property a -> Property a
(.||.) :: Property a -> Property a -> Property a
forAllM :: Gen a -> (a -> PropertyT IO ()) -> Property a
classify :: Text -> Bool -> Property a -> Property a
collect :: Show a => a -> Property b -> Property b
```

### 4. State Machine Testing

```haskell
-- State machine types
data StateMachine s c = StateMachine
    { initialState :: s
    , transitions :: ![Transition s c]
    , invariants :: ![Invariant s]
    }

data Transition s c = Transition
    { generator :: !Gen c
    , precondition :: !(s -> c -> Bool)
    , postcondition :: !(s -> c -> s -> Bool)
    , nextState :: !(s -> c -> s)
    }

-- MCP state machines
serverStateMachine :: StateMachine ServerState ServerCommand
serverStateMachine = StateMachine
    { initialState = NotStarted
    , transitions = [startServer, stopServer, handleRequest]
    , invariants = [validConnections, resourceConsistency]
    }

sessionStateMachine :: StateMachine SessionState SessionCommand
sessionStateMachine = StateMachine
    { initialState = Initializing
    , transitions = [establish, communicate, terminate]
    , invariants = [validState, cleanupResources]
    }
```

## Testing Requirements

1. Core Properties:
   - Message roundtrips
   - Version compatibility
   - Resource handling
   - Tool execution

2. State Properties:
   - Server lifecycle
   - Session management
   - Connection handling
   - Resource states

3. Integration Properties:
   - Protocol compliance
   - Transport reliability
   - Error propagation

## Files to Create/Modify
1. `src/MCP/Test/Property/Types.hs` - Core property types
2. `src/MCP/Test/Property/Generators.hs` - Test generators
3. `src/MCP/Test/Property/StateMachine.hs` - State machine testing
4. `test/MCP/Test/Property/PropertySpec.hs` - Framework tests
5. Update `mcp-core.cabal` and `mcp-server.cabal`

## Dependencies
```yaml
dependencies:
  - hedgehog
  - quickcheck
  - tasty
  - tasty-hedgehog
```

## Acceptance Criteria
1. Property types implemented
2. Generators working
3. Combinators functional
4. State machines complete
5. Shrinking effective
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. GUI test framework
2. Performance testing
3. Load testing
4. Benchmarking

## Resources
1. Hedgehog Documentation
2. Property Testing Patterns
3. State Machine Testing Examples