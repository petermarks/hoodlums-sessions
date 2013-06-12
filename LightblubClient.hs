module LightbulbClient where

import Data.Foldable (forM_)
import Network.Socket
import Control.Monad (forever)

port :: PortNumber
port = 9876

bulbHosts :: [String]
bulbHosts = [ "141.228.108.156", "141.228.108.152", "141.228.110.195", "0.0.0.0"]

main :: IO ()
main = withSocketsDo $ do
  s     <- socket AF_INET Datagram defaultProtocol
  bulbs <- mapM (fmap (SockAddrInet port) . inet_addr) bulbHosts
  setSocketOption s Broadcast 1
  forever $ do
    (r,g,b) <- readLn
    forM_ bulbs $ sendTo s $ map toEnum [r,g,b,0,0,0,0,0,0,0]
