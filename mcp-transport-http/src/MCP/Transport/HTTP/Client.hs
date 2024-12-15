{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.HTTP.Client (
    -- * HTTP Client
    HTTPClient (..),
    ClientConfig (..),
    newClient,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types

import MCP.Transport.Types

-- | HTTP client configuration
data ClientConfig = ClientConfig
    { baseUrl :: String
    , manager :: Manager
    }

-- | HTTP client state
data HTTPClient = HTTPClient
    { config :: ClientConfig
    , messageQueue :: TQueue Value
    }

-- | Create new HTTP client
newClient :: ClientConfig -> MessageHandler -> ErrorHandler -> IO Connection
newClient config onMessage onError = do
    queue <- newTQueueIO

    let client =
            HTTPClient
                { config = config
                , messageQueue = queue
                }

    -- Start message processor
    processorThread <- async $ forever $ do
        msg <- atomically $ readTQueue queue
        processMessage client msg

    -- Return connection
    return
        Connection
            { sendMessage = atomically . writeTQueue queue
            , close = cancel processorThread
            }

-- | Process outgoing message
processMessage :: HTTPClient -> Value -> IO ()
processMessage client msg = do
    let request =
            defaultRequest
                { method = "POST"
                , path = "/message"
                , requestBody = RequestBodyLBS $ encode msg
                }

    response <- httpLbs request (manager $ config client)
    unless (statusIsSuccessful $ responseStatus response) $
        throwIO $
            MessageError "Failed to send message"
