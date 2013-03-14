module XML where

import Control.Applicative
import Control.Monad
import Data.DList
import Data.Maybe
import Text.XML.Light

load :: IO Element
load = fromJust . parseXMLDoc <$> readFile "album.xml"

pp :: Element -> IO ()
pp = putStrLn . ppElement

descendants :: Element -> DList Element
descendants = fromList . elChildren >=> 
                  (\e -> return e `mplus` descendants e)

hasTag :: QName -> Element -> DList Element
hasTag n e | n == elName e = return e
           | otherwise     = mzero

qn :: String -> QName
qn s = QName s (Just "http://musicbrainz.org/ns/mmd-2.0#") Nothing

trans :: (Element -> DList a) -> Element -> [a]
trans t = toList . t

trackNames :: Element -> DList String
trackNames e = do
  e' <- descendants >=> hasTag (qn "title") $ e
  return $ strContent e'