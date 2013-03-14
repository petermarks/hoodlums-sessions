module XML where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.XML.Light

load :: IO Element
load = fromJust . parseXMLDoc <$> readFile "album.xml"

pp :: Element -> IO ()
pp = putStrLn . ppElement

descendants :: Element -> [Element]
descendants = concatMap f . elChildren
  where
    f e = e : descendants e

hasTag :: QName -> Element -> [Element]
hasTag n e | n == elName e = [e]
           | otherwise     = []

qn :: String -> QName
qn s = QName s (Just "http://musicbrainz.org/ns/mmd-2.0#") Nothing

trackNames :: Element -> [String]
trackNames e = do
  e' <- descendants >=> hasTag (qn "title") $ e
  return $ strContent e'