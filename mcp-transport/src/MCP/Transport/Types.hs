{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module MCP.Transport.Types
    ( -- * Transport Types
      Transport(..)
    , Connection(..)
    , TransportError(..)
    , MessageHandler
    , ErrorHandler
    ) where

import Data.Text (Text)
import Data.Aeson (Value)
import Control.Exception (Exception)
import GHC.Generics

-- | Handler for receiving messages
type MessageHandler = Value -> IO ()

-- | Handler for transport errors
type ErrorHandler = TransportError -> IO ()

-- | A transport connection
data Connection = Connection
    { sendMessage :: Value -> IO ()
    , close :: IO ()
    }

-- | Transport capability
class Transport t where
    -- | Start the transport with message and error handlers
    start :: t -> MessageHandler -> ErrorHandler -> IO Connection

-- | Transport-specific errors
data TransportError
    = ConnectionError Text
    | MessageError Text
    | ProtocolError Text
    deriving stock (Show, Eq, Generic)

instance Exception TransportError