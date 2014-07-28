import Network hiding (accept)
import Network.Socket (fdSocket, accept)
import Network.Socket.ByteString
import Data.List (intercalate)
import Data.ByteString.Char8 hiding (length, intercalate)
import GHC.Event
import System.Posix
import System.Posix.IO

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
    sendAll c msg
    sClose c

httpify :: String -> String
httpify content = 
    intercalate "\r\n" [
          "HTTP/1.0 200 PISS OFF"
        , "Content-Length: " ++ (show $ length content)
        , ""
        , content
        , ""]

-- Feck off, standards
html :: String -> String
html body = "<html><pre>" ++ body

msg = pack $ httpify $ html "Piss off."
