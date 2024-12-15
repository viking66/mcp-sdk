{-# LANGUAGE OverloadedStrings #-}

module MCP.Examples.FileServer (
    startFileServer,
) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
import System.FilePath

import MCP.Core
import MCP.Server
import MCP.Transport

-- | A file server that provides access to a directory through MCP
startFileServer :: FilePath -> IO ()
startFileServer root = do
    let config =
            ServerConfig
                { serverName = "file-server"
                , serverVersion = "0.1.0"
                , serverCapabilities =
                    defaultCapabilities
                        { capResources =
                            Just $
                                ResourceCapabilities
                                    { supportsListing = True
                                    , supportsReading = True
                                    , supportsSubscription = False
                                    }
                        }
                }

    server <- newServer config

    -- Add resource handlers
    void $
        addResourceProvider server $
            ResourceProvider
                { listResources = listFiles root
                , readResource = readFile' root
                , subscribe = Nothing
                }

    -- Start server
    transport <- newStdioTransport
    runServer server transport

-- | List files in directory
listFiles :: FilePath -> IO [Resource]
listFiles root = do
    files <- listDirectory root
    return
        [ Resource
            { resourceUri = T.pack $ "file://" <> file
            , resourceName = T.pack file
            , resourceDescription = Nothing
            , resourceMimeType = Just "text/plain"
            }
        | file <- files
        ]

-- | Read file contents
readFile' :: FilePath -> Text -> IO [ResourceContent]
readFile' root uri = do
    let path = root </> T.unpack (T.drop 7 uri) -- Remove "file://"
    contents <- TIO.readFile path
    return
        [ ResourceContent
            { contentUri = uri
            , contentMimeType = Just "text/plain"
            , contentText = Just contents
            , contentBlob = Nothing
            }
        ]
