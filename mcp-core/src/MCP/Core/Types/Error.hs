{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Core.Types.Error
    ( -- * Error Types
      MCPError(..)
    , ErrorCode(..)
    , ErrorData(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | An error that occurred during MCP operations
data MCPError = MCPError
    { errorCode :: ErrorCode
    , errorMessage :: Text
    , errorData :: Maybe ErrorData
    } deriving stock (Eq, Show, Generic)

instance FromJSON MCPError
instance ToJSON MCPError

-- | Standard error codes
data ErrorCode
    = ParseError         -- -32700
    | InvalidRequest     -- -32600
    | MethodNotFound     -- -32601
    | InvalidParams      -- -32602
    | InternalError      -- -32603
    | ServerError Int    -- -32000 to -32099
    deriving stock (Eq, Show, Generic)

instance FromJSON ErrorCode
instance ToJSON ErrorCode

-- | Additional error data
data ErrorData = ErrorData
    { dataTrace :: Maybe Text
    , dataContext :: Maybe Value
    } deriving stock (Eq, Show, Generic)

instance FromJSON ErrorData
instance ToJSON ErrorData