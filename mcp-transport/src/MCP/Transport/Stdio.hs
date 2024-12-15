{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Transport.Stdio
    ( -- * Stdio Transport
      StdioTransport(..)
    , newStdioTransport
    ) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

import MCP.Transport.Types

data StdioTransport = StdioTransport
    { input :: Handle
    , output :: Handle
    }

-- | Create a new stdio transport
newStdioTransport :: IO StdioTransport
newStdioTransport = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    return $ StdioTransport stdin stdout

instance Transport StdioTransport where
    start transport onMessage onError = do
        -- Create communication channels
        sendQueue <- newTQueueIO

        -- Start reader thread
        readerThread <- async $ forever $ do
            line <- TIO.hGetLine (input transport)
            case eitherDecode (encodeUtf8 line) of
                Left err -> onError $ MessageError $ T.pack err
                Right msg -> onMessage msg

        -- Start writer thread
        writerThread <- async $ forever $ do
            msg <- atomically $ readTQueue sendQueue
            TIO.hPutStrLn (output transport) (decodeUtf8 $ encode msg)

        let cleanup = do
                cancel readerThread
                cancel writerThread

        -- Return connection
        return $ Connection
            { sendMessage = atomically . writeTQueue sendQueue
            , close = cleanup
            }