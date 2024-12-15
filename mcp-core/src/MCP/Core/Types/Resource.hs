{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types.Resource (
    -- * Resource Types
    Resource (..),
    ResourceCapabilities (..),
    ResourceContent (..),
    ResourceTemplate (..),

    -- * Resource Operations
    ListResourcesRequest,
    ListResourcesResponse,
    ReadResourceRequest,
    ReadResourceResponse,
    SubscribeRequest,
    UnsubscribeRequest,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | A resource that can be accessed through MCP
data Resource = Resource
    { resourceUri :: Text
    , resourceName :: Text
    , resourceDescription :: Maybe Text
    , resourceMimeType :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Resource
instance ToJSON Resource

-- | Resource capabilities supported by a server
data ResourceCapabilities = ResourceCapabilities
    { supportsListing :: Bool
    , supportsReading :: Bool
    , supportsSubscription :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ResourceCapabilities
instance ToJSON ResourceCapabilities

-- | Content of a resource
data ResourceContent = ResourceContent
    { contentUri :: Text
    , contentMimeType :: Maybe Text
    , contentText :: Maybe Text
    , contentBlob :: Maybe Text -- Base64 encoded binary data
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ResourceContent
instance ToJSON ResourceContent

-- | Template for resource URIs
data ResourceTemplate = ResourceTemplate
    { templateUri :: Text
    , templateName :: Text
    , templateDescription :: Maybe Text
    , templateMimeType :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ResourceTemplate
instance ToJSON ResourceTemplate

-- Type aliases for request/response types (to be expanded)
type ListResourcesRequest = ()
type ListResourcesResponse = [Resource]
type ReadResourceRequest = Text -- URI
type ReadResourceResponse = [ResourceContent]
type SubscribeRequest = Text -- URI
type UnsubscribeRequest = Text -- URI
