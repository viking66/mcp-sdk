{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types.Sampling (
    -- * Sampling Types
    SamplingCapabilities (..),
    ModelPreferences (..),
    SamplingRequest (..),
    SamplingResponse (..),
    StopReason (..),

    -- * Sampling Operations
    CreateMessageRequest,
    CreateMessageResponse,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | Sampling capabilities supported by a server
data SamplingCapabilities = SamplingCapabilities
    { supportsCreateMessage :: Bool
    , supportedModelFamilies :: [Text]
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON SamplingCapabilities
instance ToJSON SamplingCapabilities

-- | Preferences for model selection
data ModelPreferences = ModelPreferences
    { modelHints :: [Text]
    , costPriority :: Double
    , speedPriority :: Double
    , intelligencePriority :: Double
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ModelPreferences
instance ToJSON ModelPreferences

-- | Request for sampling from a model
data SamplingRequest = SamplingRequest
    { requestMessages :: [PromptMessage]
    , requestModelPrefs :: Maybe ModelPreferences
    , requestSystemPrompt :: Maybe Text
    , requestIncludeContext :: Text -- "none", "thisServer", or "allServers"
    , requestTemperature :: Maybe Double
    , requestMaxTokens :: Int
    , requestStopSequences :: [Text]
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON SamplingRequest
instance ToJSON SamplingRequest

-- | Response from model sampling
data SamplingResponse = SamplingResponse
    { responseModel :: Text
    , responseStopReason :: Maybe StopReason
    , responseRole :: Text
    , responseContent :: MessageContent
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON SamplingResponse
instance ToJSON SamplingResponse

-- | Reason why sampling stopped
data StopReason
    = EndTurn
    | StopSequence
    | MaxTokens
    | Other Text
    deriving stock (Eq, Show, Generic)

instance FromJSON StopReason
instance ToJSON StopReason

-- Type aliases for request/response types
type CreateMessageRequest = SamplingRequest
type CreateMessageResponse = SamplingResponse
