{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.HTTP.Server (
    -- * HTTP Server
    HTTPServer (..),
    ServerConfig (..),
    startServer,
) where

import Control.Concurrent.STM
import Data.Aeson (Value, decode, encode)
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import MCP.Transport.Types

-- | HTTP server configuration
data ServerConfig = ServerConfig
    { port :: Int
    , host :: Text
    }

-- | HTTP server state
data HTTPServer = HTTPServer
    { config :: ServerConfig
    , messageQueue :: TQueue Value
    }

-- | Start HTTP server
startServer :: ServerConfig -> MessageHandler -> ErrorHandler -> IO Connection
startServer config onMessage onError = do
    queue <- newTQueueIO

    let server =
            HTTPServer
                { config = config
                , messageQueue = queue
                }

    -- Start Warp server
    serverThread <- async $ run (port config) $ \req respond -> case pathInfo req of
        ["message"] -> handleMessage req respond server
        _ -> respond $ responseLBS status404 [] "Not Found"

    -- Return connection
    return
        Connection
            { sendMessage = atomically . writeTQueue queue
            , close = cancel serverThread
            }

-- | Handle incoming messages
handleMessage
    :: Request
    -> (Response -> IO ResponseReceived)
    -> HTTPServer
    -> IO ResponseReceived
handleMessage req respond server = do
    body <- strictRequestBody req
    case decode body of
        Nothing -> respond $ responseLBS status400 [] "Invalid message"
        Just msg -> do
            atomically $ writeTQueue (messageQueue server) msg
            respond $ responseLBS status200 [] "OK"
