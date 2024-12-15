# Task: Implement Resource Updates and Subscriptions

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Medium (P2)  
**Package**: mcp-server
**Prerequisites**: 
- Basic resource serving (002-resource-serving)
- Protocol capability types (mcp-core/protocol/003-capability-types)

## Context
We need to implement advanced resource handling capabilities that allow clients to subscribe to resource changes and receive updates. This includes managing subscriptions, tracking changes, and efficiently delivering updates to interested clients.

## Requirements
1. Define subscription types
2. Implement change detection
3. Create update delivery
4. Support subscription management
5. Enable efficient notifications

## Detailed Implementation Plan

### 1. Subscription Types

```haskell
-- File: src/MCP/Server/Resource/Subscription.hs

-- Core subscription types
data ResourceSubscription = ResourceSubscription
    { subscriptionId :: !SubscriptionId
    , resourcePattern :: !ResourcePattern
    , subscriber :: !Subscriber
    , options :: !SubscriptionOptions
    , state :: !(TVar SubscriptionState)
    } deriving (Eq)

data SubscriptionOptions = SubscriptionOptions
    { updateMode :: !UpdateMode
    , throttle :: !(Maybe ThrottleConfig)
    , filter :: !(Maybe UpdateFilter)
    } deriving (Eq, Show)

data UpdateMode
    = FullContent      -- Send full resource content
    | ContentDiff      -- Send content differences
    | MetadataOnly     -- Send only metadata updates
    deriving (Eq, Show)
```

### 2. Change Detection

```haskell
-- Change tracking types
data ResourceChange = ResourceChange
    { changeId :: !ChangeId
    , resource :: !Resource
    , changeType :: !ChangeType
    , timestamp :: !UTCTime
    , details :: !ChangeDetails
    } deriving (Eq, Show)

data ChangeType
    = Created
    | Modified
    | Deleted
    | MetadataChanged
    deriving (Eq, Show)

-- Change detection functions
detectChanges :: Resource -> Resource -> IO [ResourceChange]
trackChanges :: Resource -> IO (Either ResourceError ChangeTracker)
computeDiff :: Resource -> Resource -> IO (Maybe ContentDiff)
```

### 3. Update Delivery

```haskell
-- Update handling
data ResourceUpdate = ResourceUpdate
    { updateId :: !UpdateId
    , subscription :: !SubscriptionId
    , changes :: ![ResourceChange]
    , batchInfo :: !BatchInfo
    } deriving (Eq, Show)

data BatchInfo = BatchInfo
    { batchId :: !BatchId
    , sequence :: !Int
    , complete :: !Bool
    } deriving (Eq, Show)

-- Delivery functions
deliverUpdate :: ResourceUpdate -> Subscriber -> IO (Either DeliveryError ())
batchUpdates :: [ResourceUpdate] -> BatchConfig -> IO [BatchedUpdate]
retryDelivery :: DeliveryError -> ResourceUpdate -> IO (Either DeliveryError ())
```

### 4. Subscription Management

```haskell
-- Management types
data SubscriptionManager = SubscriptionManager
    { subscriptions :: !(TVar (Map SubscriptionId ResourceSubscription))
    , policies :: !SubscriptionPolicies
    , metrics :: !(TVar SubscriptionMetrics)
    } deriving (Eq)

data SubscriptionPolicies = SubscriptionPolicies
    { maxSubscriptions :: !Int
    , maxUpdatesPerSecond :: !Int
    , defaultThrottle :: !ThrottleConfig
    } deriving (Eq, Show)

-- Management functions
addSubscription :: ResourceSubscription -> SubscriptionManager -> IO (Either SubscriptionError ())
removeSubscription :: SubscriptionId -> SubscriptionManager -> IO ()
updateSubscription :: SubscriptionId -> SubscriptionOptions -> SubscriptionManager -> IO (Either SubscriptionError ())
```

## Testing Requirements

1. Unit Tests:
   - Change detection
   - Update delivery
   - Subscription handling
   - Error scenarios

2. Integration Tests:
   - With resource system
   - Multiple subscribers
   - Update batching

3. Property Tests:
   - Change tracking
   - Delivery ordering
   - Subscription states

## Files to Create/Modify
1. `src/MCP/Server/Resource/Subscription.hs` - Subscription system
2. `src/MCP/Server/Resource/Change.hs` - Change tracking
3. `src/MCP/Server/Resource/Update.hs` - Update delivery
4. `test/MCP/Server/Resource/SubscriptionSpec.hs` - Tests
5. Update `mcp-server.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - stm
  - containers
  - time
  - async
```

## Acceptance Criteria
1. Subscription system working
2. Change detection accurate
3. Updates delivered reliably
4. Management functional
5. Error handling robust
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Complex diff algorithms
2. Update compression
3. Historic changes
4. Update persistence

## Resources
1. Change Detection Patterns
2. Subscription Management Examples
3. Update Delivery Strategies