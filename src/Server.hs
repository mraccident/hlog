{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server
    ( serve
    , response
    , reqUri
    ) where

import Prelude hiding (length, intercalate, readFile)
import Data.ByteString.Char8
import Network hiding (accept)
import Network.Socket hiding (sClose, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Text.Regex.PCRE
import System.Posix.Env.ByteString
import Ecumenical

serve :: PortNumber -> IO ()
serve port = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    loop sock

loop sock = do
    (conn, _) <- accept sock
    forkIO $ body conn
    loop sock
    where
        body conn = do
            req <- recv conn 4096

            -- Print incoming request information
            peer <- getPeerName conn
            Prelude.putStrLn $ show peer ++ ": " ++ show req
            Prelude.putStrLn $ "Requested URI: " ++ show (reqUri req)
            resp <- handleRequest req
            sendAll conn $ resp
            sClose conn

handleRequest :: ByteString -> IO ByteString
handleRequest request = case (reqUri request) of
    Nothing -> return $ response "400 NEED A DRINK" page400
    Just uri -> do
        -- Treat URIs starting with static/ as requests for static files;
        -- everything else goes to the as yet nonexistent blog.
        case (stripPrefix "/static/" uri) of
            Just path -> serveFile path
            Nothing -> do
                value <- retrieve uri
                case value of
                    Just value -> return $ response "200 DRINK" value
                    Nothing -> return $ response "404 THE FECK" page404

staticFilesPath :: IO ByteString
staticFilesPath = do
    args <- getArgs
    return $ case args of
        [path] -> path
        [] -> "static"
        _ -> "static"

-- This typeclass abstracts the IO-dependent static file server
class Monad m => StaticFileServer m where
    serveFile :: ByteString -> m ByteString

instance StaticFileServer IO where
    serveFile = serveStatic

newtype MockStaticFileServer m a = MockStaticFileServer
    { whyDoesThisNeedAName :: ReaderT (ByteString -> ByteString) m a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadTrans
             , MonadReader (ByteString -> ByteString)
             )

runMockServer :: MockStaticFileServer m a -> (ByteString -> ByteString) -> m a
runMockServer (MockStaticFileServer s) = runReaderT s

-- Serve static file
serveStatic :: ByteString -> IO ByteString
serveStatic path = do
    prefix <- staticFilesPath
    result <- fileContents (prefix `append` "/" `append` path)
    case result of
        Nothing -> return $ response "404 NO TEA" page404
        Just garbage -> return $ response "200 ARSE" garbage

page400 :: ByteString
page400 = "<html><center><h1>400 NEED A DRINK</h1><hr/>\
            \How did that <em>gobshite</em> get on the socket?!</html>"

page403 :: ByteString
page403 = "<html><center><h1>403 Feck Off, Cup!</h1><hr/>\
            \And what do you say to a cup of tea?</html>"

page404 :: ByteString
page404 = "<html><center><h1>404 Shut Up Dougal</h1><hr/>\
            \One last time. These packets are <em>small</em>, but the ones \
            \out there are <em>far away</em>.</html>"

reqUri :: ByteString -> Maybe ByteString
reqUri r = group1 $ ((r =~ pattern) :: [[ByteString]])
    where pattern = "GET ([^ ]+) HTTP/1\\.1" :: ByteString
          group1 :: [[ByteString]] -> Maybe ByteString
          group1 [[_, x]] = Just x
          group1 _ = Nothing

fileContents :: ByteString -> IO (Maybe ByteString)
fileContents path = do
    -- XXX: this annotation is annoying, please slay it
    -- XXX: also, whytf does ByteString.readFile take a [Char]???
    contents <- (try $ readFile $ unpack path) :: IO (Either IOException ByteString)
    case contents  of
        Left _ -> return Nothing
        Right text -> return $ Just text

response :: ByteString -> ByteString -> ByteString
response status body =
    intercalate "\r\n" [
          "HTTP/1.1 " `append` status
        , "Content-Length: " `append` (pack $ show $ length body)
        , ""
        , body]
