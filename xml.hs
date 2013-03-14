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

type Trans a = Cursor -> DList a

children :: Trans Cursor
children (current -> Elem e) = 
  fromList $ fmap fromElement $ elChildren e
children _ = mzero

descendants :: Trans Cursor
descendants = children >=> (\c -> return c `mplus` descendants c)

hasTag :: QName -> Trans Cursor
hasTag n c@(current -> Elem e) | n == elName e = return c
hasTag _ _ = mzero

getText :: Trans String
getText (current -> Elem e) = return $ strContent e
getText _ = mzero

elem :: QName -> DList Content -> DList Content
elem n = return . Elem . node n . toList

text :: String -> DList Content
text s = return . Text $ CData CDataText s Nothing

qn :: String -> QName
qn s = QName s (Just "http://musicbrainz.org/ns/mmd-2.0#") Nothing

trans :: Trans a -> Element -> [a]
trans t = toList . t . fromElement

trackNames :: Trans Content
trackNames e = elem (qn "titles") $ do
  t <- descendants >=> hasTag (qn "title") >=> getText $ e
  elem (qn "title") $ text t
