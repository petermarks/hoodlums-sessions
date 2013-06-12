module LightBulb where

import Data.Char
import Network.Socket
import Graphics.Gloss.Interface.IO.Animate

port :: PortNumber
port = 9876  

host :: String
host = "0.0.0.0"

type Host = SockAddr

main :: IO ()
main = withSocketsDo $ do
  s <- socket AF_INET Datagram defaultProtocol
  bindAddr <- inet_addr host
  bindSocket s (SockAddrInet port bindAddr)
  animateFixedIO (InWindow "LightBulb" (500,500) (20,20)) (makeColor 0 0 0 1) $ \_ -> do
    (msg, _, _) <- recvFrom s 1024
    let (r:g:b:_) = map ord msg
    return $ color (makeColor8 r g b 255)$ rectangleSolid 500 500
