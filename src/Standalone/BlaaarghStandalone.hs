{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude hiding (catch)
import           Control.Exception
import           Happstack.Server
import           System.Environment
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.IO


import           Blaaargh


getBlaaarghDir :: IO FilePath
getBlaaarghDir = getEnv "BLAAARGH_DIR" `catch`
                   \(_ :: SomeException) -> return "."


main :: IO ()
main = do
    -- FIXME: parse arguments, usage statement
    bs <- getBlaaarghDir >>= initBlaaargh

    loghandler <- streamHandler stdout DEBUG

    updateGlobalLogger rootLoggerName
                       (setLevel DEBUG . setHandlers [loghandler])

    let handler = runBlaaarghHandler bs $ serveBlaaargh

    --runFastCGIConcurrent 10 $ serverPartToCGI handler
    simpleHTTP (nullConf {port=5000}) handler
