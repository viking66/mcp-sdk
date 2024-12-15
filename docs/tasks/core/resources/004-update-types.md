# Task: Implement Resource Update Types

**Status**: 🔴 Not Started  
**Assignee**: Unassigned  
**Estimated effort**: 1 day (SDE2 level)  
**Priority**: Highest (P0)  
**Package**: mcp-core
**Prerequisites**: 
- Core resource types (001-resource-types)
- Resource content types (002-content-types)
- Resource metadata types (003-metadata-types)

## Context
We need to implement types for handling resource updates, including change tracking, update notifications, and subscription management. This system needs to support both full resource updates and partial changes while maintaining consistency.

## Requirements
1. Define update event types
2. Implement change tracking
3. Create subscription types
4. Add notification handling
5. Support update validation

## Detailed Implementation Plan

### 1. Update Event Types

```haskell
-- File: src/MCP/Core/Resource/Update.hs

-- Core update type
data ResourceUpdate = ResourceUpdate
    { updateId :: !UpdateId
    , resourceId :: !ResourceId
    , updateType :: !UpdateType
    , timestamp :: !UTCTime
    , changes :: ![ResourceChange]
    } deriving (Eq, Show)

-- Update identification
newtype UpdateId = UpdateId 
    { unUpdateId :: UUID }
    deriving (Eq, Show, Ord)

-- Types of updates
data UpdateType
    = ContentUpdate
    | MetadataUpdate
    | FullUpdate
    | DeleteUpdate
    deriving (Eq, Show)

-- Change tracking
data ResourceChange
    = ContentChange !ContentDiff
    | MetadataChange !MetadataDiff
    | StatusChange !StatusDiff
    deriving (Eq, Show)
```

### 2. Subscription System

```haskell
-- Subscription types
data Subscription = Subscription
    { subscriptionId :: !SubscriptionId
    , resourcePattern :: !ResourcePattern
    , filters :: ![UpdateFilter]
    , callback :: UpdateCallback
    } deriving (Eq)

newtype SubscriptionId = SubscriptionId 
    { unSubscriptionId :: UUID }
    deriving (Eq, Show, Ord)

-- Subscription matching
data ResourcePattern
    = ExactResource !ResourceId
    | PrefixMatch !Text
    | PatternMatch !Text
    | AllResources
    deriving (Eq, Show)

-- Update filtering
data UpdateFilter
    = TypeFilter ![UpdateType]
    | TimeFilter !TimeRange
    | ChangeFilter ![ChangeType]
    deriving (Eq, Show)
```

### 3. Notification System

```haskell
-- Notification types
data UpdateNotification = UpdateNotification
    { notificationId :: !NotificationId
    , subscription :: !SubscriptionId
    , updates :: ![ResourceUpdate]
    , batchSize :: !Int
    , complete :: !Bool
    } deriving (Eq, Show)

-- Notification delivery
type UpdateCallback = UpdateNotification -> IO ()

data NotificationPreferences = NotificationPreferences
    { batchSize :: !Int
    , maxLatency :: !NominalDiffTime
    , ordering :: !UpdateOrdering
    } deriving (Eq, Show)

data UpdateOrdering
    = Chronological
    | BatchOptimized
    | PriorityBased !PriorityMap
    deriving (Eq, Show)
```

### 4. Change Tracking

```haskell
-- Change tracking types
data ContentDiff
    = FullContent !Content
    | IncrementalContent !DiffOps
    deriving (Eq, Show)

data MetadataDiff
    = FullMetadata !Metadata
    | FieldUpdates ![FieldUpdate]
    deriving (Eq, Show)

data StatusDiff
    = StatusChange !ResourceStatus !ResourceStatus  -- Old, New
    | StateTransition !StateTransition
    deriving (Eq, Show)

-- Update operations
data DiffOps
    = Insert !Int !ByteString
    | Delete !Int !Int
    | Replace !Int !Int !ByteString
    | Copy !Int !Int
    deriving (Eq, Show)
```

## Testing Requirements

1. Property Tests:
   - Update generation
   - Subscription matching
   - Change tracking

2. Unit Tests:
   - All update types
   - Notification delivery
   - Filter application

3. Integration Tests:
   - With resource system
   - Update processing
   - Subscription handling

## Files to Create/Modify
1. `src/MCP/Core/Resource/Update.hs` - Core update types
2. `src/MCP/Core/Resource/Subscription.hs` - Subscription system
3. `src/MCP/Core/Resource/Notification.hs` - Notification handling
4. `test/MCP/Core/Resource/UpdateSpec.hs` - Tests
5. Update `mcp-core.cabal` with new modules

## Dependencies
```yaml
dependencies:
  - uuid
  - time
  - stm
  - async
```

## Acceptance Criteria
1. Update types implemented
2. Subscription system working
3. Notification delivery functional
4. Change tracking accurate
5. Filter system working
6. Full test coverage
7. Documentation complete
8. Code passes style checks

## Non-Goals
1. Update persistence
2. Complex diff algorithms
3. Update compression
4. Historical tracking

## Resources
1. Change Notification Patterns
2. Subscription System Design
3. Diff Algorithm Examples