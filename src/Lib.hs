{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( serve,
      reqUri
    ) where

import Prelude hiding (length, intercalate)
import Data.ByteString.Char8
import Network hiding (accept)
import Network.Socket hiding (sClose)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
import Text.Regex.PCRE

serve :: PortNumber -> IO ()
serve port = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    loop sock

loop sock = do
    (conn, _) <- accept sock

    -- Print incoming request information
    req <- recv conn 4096
    peer <- getPeerName conn
    Prelude.putStrLn $ show peer ++ ": " ++ show req

    forkIO $ body conn
    loop sock
    where
        body c = do
            sendAll c msg
            sClose c

reqUri :: String -> Maybe String
reqUri r = group1 $ ((r =~ pattern) :: [[String]])
    where pattern = "GET ([^ ]+) HTTP/1\\.1" :: String
          group1 :: [[String]] -> Maybe String
          group1 [[_, x]] = Just x
          group1 _ = Nothing

httpify :: ByteString -> ByteString
httpify content =
    intercalate "\r\n" [
          "HTTP/1.0 200 PISS OFF"
        , "Content-Length: " `append` (pack $ show $ length content)
        , ""
        , content]

msg :: ByteString
msg = httpify "Piss off."
