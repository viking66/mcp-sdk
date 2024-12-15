{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.HTTP.SSE (
    -- * SSE Support
    SSEConfig (..),
    addSSESupport,
) where

import Control.Concurrent.STM
import Data.Aeson
import Network.HTTP.Types
import Network.Wai

-- | SSE configuration
data SSEConfig = SSEConfig
    { eventQueue :: TQueue Value
    , keepAliveInterval :: Int -- microseconds
    }

-- | Add SSE support to a WAI application
addSSESupport :: SSEConfig -> Application -> Application
addSSESupport config app req respond
    | pathInfo req == ["events"] = handleSSE config respond
    | otherwise = app req respond

-- | Handle SSE connection
handleSSE
    :: SSEConfig -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSSE config respond = do
    -- Set up response stream
    let stream :: StreamingBody
        stream write flush = do
            -- Send initial response
            write "retry: 1000\n\n"
            flush

            -- Start event loop
            forever $ do
                event <- atomically $ readTQueue (eventQueue config)
                write $ "data: " <> encode event <> "\n\n"
                flush

                -- Keep-alive
                threadDelay (keepAliveInterval config)
                write ":\n\n" -- Comment for keep-alive
                flush

    respond $
        responseStream
            status200
            [ ("Content-Type", "text/event-stream")
            , ("Cache-Control", "no-cache")
            , ("Connection", "keep-alive")
            ]
            stream
