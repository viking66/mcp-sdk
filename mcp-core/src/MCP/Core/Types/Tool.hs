{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types.Tool (
    -- * Tool Types
    Tool (..),
    ToolCapabilities (..),
    ToolSchema (..),
    ToolResult (..),

    -- * Tool Operations
    ListToolsRequest,
    ListToolsResponse,
    CallToolRequest,
    CallToolResponse,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | A tool that can be called through MCP
data Tool = Tool
    { toolName :: Text
    , toolDescription :: Maybe Text
    , toolInputSchema :: ToolSchema
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON Tool
instance ToJSON Tool

-- | Tool capabilities supported by a server
data ToolCapabilities = ToolCapabilities
    { supportsListTools :: Bool
    , supportsCallTool :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ToolCapabilities
instance ToJSON ToolCapabilities

-- | JSON Schema for tool input validation
newtype ToolSchema = ToolSchema Value
    deriving stock (Eq, Show, Generic)

instance FromJSON ToolSchema
instance ToJSON ToolSchema

-- | Result of a tool execution
data ToolResult = ToolResult
    { resultContent :: [MessageContent]
    , resultIsError :: Bool
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ToolResult
instance ToJSON ToolResult

-- Type aliases for request/response types (to be expanded)
type ListToolsRequest = ()
type ListToolsResponse = [Tool]
type CallToolRequest = (Text, Value) -- (name, arguments)
type CallToolResponse = ToolResult
