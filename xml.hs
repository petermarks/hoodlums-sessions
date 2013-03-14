module XML where

import Control.Applicative
import Data.Maybe
import Text.XML.Light

load :: IO Element
load = fromJust . parseXMLDoc <$> readFile "album.xml"

pp :: Element -> IO ()
pp = putStrLn . ppElement