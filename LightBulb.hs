module LightBulb where

import Data.Char
import Data.IORef
import Network.Socket
import Graphics.Gloss.Interface.IO.Animate
import Control.Monad (forever, void)
import Control.Concurrent

port :: PortNumber
port = 9876  

host :: String
host = "0.0.0.0"

type Host = SockAddr

main :: IO ()
main = withSocketsDo $ do
  s        <- socket AF_INET Datagram defaultProtocol
  bindAddr <- inet_addr host
  bindSocket s (SockAddrInet port bindAddr)
  colorRef <- newIORef $ makeColor8 0 0 0 1
  void $ forkIO $ forever $ do
    (msg, _, _) <- recvFrom s 1024
    let (r:g:b:_) = map ord msg
    writeIORef colorRef $ makeColor8 r g b 255
  animateFixedIO (InWindow "LightBulb" (500,500) (20,20)) (makeColor 0 0 0 1) $ \_ -> do
    c <- readIORef colorRef
    return $ color c $ rectangleSolid 500 500
