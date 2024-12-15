{-# LANGUAGE OverloadedStrings #-}

module MCP.Examples.SimpleServer
    ( startSimpleServer
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import MCP.Core
import MCP.Server
import MCP.Transport

-- | A simple echo server that demonstrates basic MCP functionality
startSimpleServer :: IO ()
startSimpleServer = do
    let config = ServerConfig
            { serverName = "simple-server"
            , serverVersion = "0.1.0"
            , serverCapabilities = defaultCapabilities
            }

    server <- newServer config
    transport <- newStdioTransport
    
    runServer server transport