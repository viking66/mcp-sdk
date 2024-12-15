{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types (
    -- * Core Protocol Types
    MCPVersion (..),
    Capabilities (..),
    ServerConfig (..),
    ClientConfig (..),
    ServerInfo (..),
    ClientInfo (..),

    -- * Re-exports
    module MCP.Core.Types.Resource,
    module MCP.Core.Types.Prompt,
    module MCP.Core.Types.Tool,
    module MCP.Core.Types.Sampling,
    module MCP.Core.Types.Error,
) where

import Data.Aeson
import Data.Text (Text)
import Data.Version
import GHC.Generics

import MCP.Core.Types.Error
import MCP.Core.Types.Prompt
import MCP.Core.Types.Resource
import MCP.Core.Types.Sampling
import MCP.Core.Types.Tool

-- | MCP Protocol Version
newtype MCPVersion = MCPVersion Version
    deriving stock (Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Server capabilities
data Capabilities = Capabilities
    { capResources :: Maybe ResourceCapabilities
    , capPrompts :: Maybe PromptCapabilities
    , capTools :: Maybe ToolCapabilities
    , capSampling :: Maybe SamplingCapabilities
    }
    deriving (Eq, Show, Generic)

instance FromJSON Capabilities
instance ToJSON Capabilities

-- | Server configuration
data ServerConfig = ServerConfig
    { serverName :: Text
    , serverVersion :: Text
    , serverCapabilities :: Capabilities
    }
    deriving (Eq, Show, Generic)

instance FromJSON ServerConfig
instance ToJSON ServerConfig

-- | Client configuration
data ClientConfig = ClientConfig
    { clientName :: Text
    , clientVersion :: Text
    , clientCapabilities :: Capabilities
    }
    deriving (Eq, Show, Generic)

instance FromJSON ClientConfig
instance ToJSON ClientConfig

-- | Server information exchanged during initialization
data ServerInfo = ServerInfo
    { serverProtocolVersion :: MCPVersion
    , serverConfig :: ServerConfig
    }
    deriving (Eq, Show, Generic)

instance FromJSON ServerInfo
instance ToJSON ServerInfo

-- | Client information exchanged during initialization
data ClientInfo = ClientInfo
    { clientProtocolVersion :: MCPVersion
    , clientConfig :: ClientConfig
    }
    deriving (Eq, Show, Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo
