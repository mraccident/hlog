{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( serve
    , response
    , reqUri
    ) where

import Prelude hiding (length, intercalate, readFile)
import Data.ByteString.Char8
import Network hiding (accept)
import Network.Socket hiding (sClose)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
import Control.Exception
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
    Prelude.putStrLn $ "Requested URI: " ++ show (reqUri req)

    forkIO $ body conn req
    loop sock
    where
        body c r = do
            resp <- serveStatic r
            sendAll c $ resp
            sClose c

reqUri :: String -> Maybe String
reqUri r = group1 $ ((r =~ pattern) :: [[String]])
    where pattern = "GET /([^ ]+) HTTP/1\\.1" :: String
          group1 :: [[String]] -> Maybe String
          group1 [[_, x]] = Just x
          group1 _ = Nothing

-- Serve static file
serveStatic :: String -> IO ByteString
serveStatic request = do
    case (reqUri request) of
        Nothing -> return $ response "400 NEED A DRINK" ""
        Just uri -> do
            result <- fileContents uri
            case result of
                Nothing -> return $ response "404 FECK OFF" ""
                Just garbage -> return $ response "200 ARSE" garbage

fileContents :: FilePath -> IO (Maybe ByteString)
fileContents path = do
    -- XXX: this annotation is annoying, please slay it
    contents <- (try $ readFile path) :: IO (Either IOException ByteString)
    case contents  of
        Left _ -> return Nothing
        Right text -> return $ Just text

-- Handle HTTP request
handleReq :: String -> ByteString
handleReq r = handleUri $ reqUri r
    where
        handleUri :: Maybe String -> ByteString
        handleUri Nothing = response "400 FECK OFF"
            "<h1>HTTP 400: THAT WOULD BE AN ECUMENICAL MATTER.<h1><hr/>"
        handleUri (Just x) = response "404 ARSE" (
            "<h1>HTTP 404 Down With This Sort Of Thing</h1><hr/>\
            \If there were a document at <code>"
            `append` (pack x) `append`
            "</code>, I would serve it to you. Alas, there is not.")

response :: ByteString -> ByteString -> ByteString
response status body =
    intercalate "\r\n" [
          "HTTP/1.1 " `append` status
        , "Content-Length: " `append` (pack $ show $ length body)
        , ""
        , body]
