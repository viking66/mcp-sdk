{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types.Prompt (
    -- * Prompt Types
    Prompt (..),
    PromptCapabilities (..),
    PromptArgument (..),
    PromptMessage (..),
    MessageContent (..),

    -- * Prompt Operations
    ListPromptsRequest,
    ListPromptsResponse,
    GetPromptRequest,
    GetPromptResponse,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | A prompt template that can be used through MCP
data Prompt = Prompt
    { promptName :: Text
    , promptDescription :: Maybe Text
    , promptArguments :: [PromptArgument]
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Prompt
instance ToJSON Prompt

-- | Prompt capabilities supported by a server
data PromptCapabilities = PromptCapabilities
    { supportsListPrompts :: Bool
    , supportsGetPrompt :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON PromptCapabilities
instance ToJSON PromptCapabilities

-- | An argument for a prompt template
data PromptArgument = PromptArgument
    { argumentName :: Text
    , argumentDescription :: Maybe Text
    , argumentRequired :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON PromptArgument
instance ToJSON PromptArgument

-- | A message in a prompt template
data PromptMessage = PromptMessage
    { messageRole :: Text -- "user" or "assistant"
    , messageContent :: MessageContent
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON PromptMessage
instance ToJSON PromptMessage

-- | Content of a prompt message
data MessageContent
    = TextContent
        { contentType :: Text
        , text :: Text
        }
    | ResourceContent Resource
    deriving stock (Eq, Show, Generic)

instance FromJSON MessageContent
instance ToJSON MessageContent

-- Type aliases for request/response types (to be expanded)
type ListPromptsRequest = ()
type ListPromptsResponse = [Prompt]
type GetPromptRequest = (Text, [(Text, Text)]) -- (name, arguments)
type GetPromptResponse = [PromptMessage]
