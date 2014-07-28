{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (length, intercalate)
import Network hiding (accept)
import Network.Socket (fdSocket, accept, getPeerName)
import Network.Socket.ByteString
import Data.ByteString.Char8
import GHC.Event
import System.Posix hiding (append)
import System.Posix.IO hiding (append)

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 1337
    let fd = fromIntegral (fdSocket sock)
    mgr <- new
    registerFd mgr (client sock) fd evtRead
    loop mgr

client :: Socket -> FdKey -> Event -> IO ()
client sock _ _ = do
    (c, _) <- accept sock
    req <- recv c 4096
    peer <- getPeerName c
    Prelude.putStrLn $ show peer ++ ": " ++ show req
    sendAll c msg
    sClose c

httpify :: ByteString -> ByteString
httpify content = 
    intercalate "\r\n" [
          "HTTP/1.0 200 PISS OFF"
        , "Content-Length: " `append` (pack $ show $ length content)
        , ""
        , content]

msg :: ByteString
msg = httpify "Piss off."
