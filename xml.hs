{-# language ViewPatterns #-}

module XML where

import Prelude hiding (elem)
import Control.Applicative
import Control.Monad
import Data.DList
import Data.Maybe
import Text.XML.Light
import Text.XML.Light.Cursor

load :: IO Element
load = fromJust . parseXMLDoc <$> readFile "album.xml"

pp :: [Content] -> IO ()
pp = mapM_ $ putStrLn . ppContent

type Trans a b = a -> DList b

children :: Trans Cursor Cursor
children (current -> Elem e) = 
  fromList $ fmap fromElement $ elChildren e
children _ = mzero

descendants :: Trans Cursor Cursor
descendants = children >=> (\c -> return c `mplus` descendants c)

hasTag :: QName -> Trans Cursor Cursor
hasTag n c@(current -> Elem e) | n == elName e = return c
hasTag _ _ = mzero

getText :: Trans Cursor String
getText (current -> Elem e) = return $ strContent e
getText _ = mzero

elem :: QName -> Trans a Content -> Trans a Content
elem n c = return . Elem . node n . toList . c

text :: Trans String Content
text s = return . Text $ CData CDataText s Nothing

qn :: String -> QName
qn s = QName s (Just "http://musicbrainz.org/ns/mmd-2.0#") Nothing

trans :: Trans Cursor a -> Element -> [a]
trans t = toList . t . fromElement

trackNames :: Trans Cursor Content
trackNames =
  elem (qn "titles") $
    descendants >=> hasTag (qn "title") >=> getText >=>
      elem (qn "title") text
