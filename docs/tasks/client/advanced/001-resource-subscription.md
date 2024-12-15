# Task: Implement Resource Subscription

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: mcp-client
**Prerequisites**: 
- Resource reading (002-resource-reading)
- Connection management (005-connection-management)

## Context
We need to implement client-side resource subscription capabilities to allow clients to receive updates when resources change. This includes subscription management, update handling, and proper cleanup of subscriptions.

## Requirements
1. Define subscription types
2. Implement update handling
3. Create subscription management
4. Support error recovery
5. Enable subscription cleanup

## Detailed Implementation Plan

### 1. Subscription Types

```haskell
-- File: src/MCP/Client/Resource/Subscription.hs

-- Core subscription types
data ResourceSubscriber = ResourceSubscriber
    { client :: !Client
    , subscriptions :: !(TVar (Map SubscriptionId Subscription))
    , handlers :: !(TVar (Map ResourcePattern UpdateHandler))
    , config :: !SubscriberConfig
    } deriving (Eq)

data Subscription = Subscription
    { subscriptionId :: !SubscriptionId
    , pattern :: !ResourcePattern
    , handler :: !UpdateHandler
    , options :: !SubscriptionOptions
    , state :: !(TVar SubscriptionState)
    } deriving (Eq)

data SubscriptionOptions = SubscriptionOptions
    { bufferSize :: !Int
    , retryPolicy :: !RetryPolicy
    , updateMode :: !UpdateMode
    } deriving (Eq, Show)
```

### 2. Update Handling

```haskell
-- Update types
data ResourceUpdate = ResourceUpdate
    { resource :: !Resource
    , changeType :: !ChangeType
    , timestamp :: !UTCTime
    , metadata :: !UpdateMetadata
    } deriving (Eq, Show)

data UpdateHandler = UpdateHandler
    { onUpdate :: !(ResourceUpdate -> IO ())
    , onError :: !(UpdateError -> IO ())
    , onComplete :: !(IO ())
    }

-- Update functions
handleUpdate :: ResourceUpdate -> ResourceSubscriber -> IO ()
processUpdate :: ResourceUpdate -> Subscription -> IO (Either UpdateError ())
validateUpdate :: ResourceUpdate -> SubscriptionOptions -> Either ValidationError ()
```

### 3. Subscription Management

```haskell
-- Management types
data SubscriptionRequest = SubscriptionRequest
    { pattern :: !ResourcePattern
    , handler :: !UpdateHandler
    , options :: !SubscriptionOptions
    } deriving (Eq, Show)

data SubscriptionState
    = Active
    | Paused
    | Reconnecting !Int
    | Failed !SubscriptionError
    deriving (Eq, Show)

-- Management functions
subscribe :: SubscriptionRequest -> ResourceSubscriber -> IO (Either SubscriptionError Subscription)
unsubscribe :: SubscriptionId -> ResourceSubscriber -> IO ()
pauseSubscription :: SubscriptionId -> ResourceSubscriber -> IO ()
resumeSubscription :: SubscriptionId -> ResourceSubscriber -> IO ()
```

### 4. Error Recovery

```haskell
-- Recovery types
data RecoveryAction
    = Retry !RetryConfig
    | Reconnect !ReconnectConfig
    | ResetSubscription
    | FailSubscription !Text
    deriving (Eq, Show)

data RetryConfig = RetryConfig
    { maxAttempts :: !Int
    , backoffStrategy :: !BackoffStrategy
    , timeout :: !NominalDiffTime
    } deriving (Eq, Show)

-- Recovery functions
handleSubscriptionError :: SubscriptionError -> Subscription -> IO RecoveryAction
attemptRecovery :: RecoveryAction -> Subscription -> IO (Either SubscriptionError ())
resetSubscriptionState :: Subscription -> IO ()
```

## Testing Requirements

1. Unit Tests:
   - Subscription lifecycle
   - Update handling
   - Error recovery
   - State management

2. Integration Tests:
   - With server
   - Multiple subscriptions
   - Recovery scenarios

3. Property Tests:
   - Update ordering
   - State transitions
   - Error handling

## Files to Create/Modify
1. `src/MCP/Client/Resource/Subscription.hs` - Core subscription
2. `src/MCP/Client/Resource/Update.hs` - Update handling
3. `src/MCP/Client/Resource/Recovery.hs` - Error recovery
4. `test/MCP/Client/Resource/SubscriptionSpec.hs` - Tests
5. Update `mcp-client.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - async
  - time
  - retry
```

## Acceptance Criteria
1. Subscription working
2. Updates handled correctly
3. Management functional
4. Recovery working
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex filtering
2. Update persistence
3. Batch updates
4. Update compression

## Resources
1. Subscription Pattern Examples
2. Recovery Strategy Patterns
3. Error Handling Guidelines