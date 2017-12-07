module Main where

import Prelude
import Data.Maybe
import Graphics.Canvas as C
import Control.Monad.Eff
import Partial.Unsafe
import Data.Traversable
import Data.List
import Data.Int

drawImage :: (Partial) => Maybe C.CanvasImageSource -> Eff (canvas :: C.CANVAS) Unit
drawImage Nothing = pure unit
drawImage (Just img) = do
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  ctx <- traverse (blob ctx) (0 .. 3)
  pure unit
  where
    blob ctx i = C.drawImage ctx img (toNumber i * 256.0) 0.0


main :: Eff (canvas :: C.CANVAS) Unit
main = unsafePartial $ C.tryLoadImage "https://i.stack.imgur.com/xlVbG.png" drawImage
