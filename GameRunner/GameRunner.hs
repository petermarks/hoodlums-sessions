module GameRunner where

import Control.Monad
import Network.Socket
import Text.Printf

data Player = Player String HostAddress PortNumber deriving (Show)

main :: IO ()
main = withSocketsDo $ do
  players <- loadPlayers "players.txt"
  playRound players

loadPlayers :: FilePath -> IO [Player]
loadPlayers path = do
  file <- readFile path
  forM (takeWhile (not . null) $ lines file) $ \line -> do
    let [name, ip, port] = words line
    addr <- inet_addr ip
    return $ Player name addr (fromIntegral $ read port)


playRound :: [Player] -> IO ()
playRound ps = do
  ss <- openSockets ps
  let pairs = [(p, q) | p <- ss, q <- ss, p /= q]
  forM_ pairs $ \((name1, sock1), (name2, sock2)) -> do
    send sock1 "play"
    answer1 <- recv sock1 1024
    send sock2 "play"
    answer2 <- recv sock2 1024
    let w = winner answer1 answer2
    case w of
      First -> printf "%s wins\n" name1
      Second -> printf "%s wins\n" name2
      Draw -> printf "%s and %s draw\n" name1 name2
  closeSockets ss

openSockets :: [Player] -> IO [(String, Socket)]
openSockets ps = forM ps $ \(Player name host port) -> do
  putStrLn name
  sock <- socket AF_INET Stream defaultProtocol
  let sockAddr = SockAddrInet port host
  connect sock sockAddr
  putStrLn "opened"
  return (name, sock)

closeSockets :: [(String, Socket)] -> IO ()
closeSockets = mapM_ (sClose . snd)

data Result = First | Second | Draw
  deriving (Show)

winner :: String -> String -> Result
winner "ROCK" "SCISSORS"  = First
winner "SCISSORS" "PAPER" = First
winner "PAPER" "ROCK"     = First
winner x y | x == y       = Draw
winner _ _                = Second