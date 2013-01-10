module Lenses where

import Prelude hiding ((.), id)
import Control.Category

data Lens s a = Lens (s -> a) ((a -> a) -> s -> s)

instance Category Lens where
  id = Lens id id
  (Lens lg lm) . (Lens rg rm) = Lens (lg . rg) (rm . lm)

(^.) :: s -> Lens s a -> a
s ^. (Lens f _) = f s

(%=) :: Lens s a -> (a -> a) -> s -> s
((Lens _ m) %= f) s = m f s

(^=) :: Lens s a -> a -> s -> s
l ^= a = l %= (const a)

infixl 8 ^.
infixr 4 %=
infixr 4 ^=


data Point = Point {x :: Double, y :: Double}
	deriving (Eq, Show)

xLens :: Lens Point Double
xLens = Lens x (\f p -> p{x= f (x p)})

yLens :: Lens Point Double
yLens = Lens y (\f p -> p{y= f (y p)})

data Circle = Circle {origin::Point, radius :: Double}
  deriving (Eq, Show)

radiusLens :: Lens Circle Double
radiusLens = Lens radius (\f c -> c{radius=f (radius c)})

originLens :: Lens Circle Point
originLens = Lens origin (\f c -> c{origin=f (origin c)})

