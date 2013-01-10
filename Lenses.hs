module Lenses where

data Point = Point {x :: Double, y :: Double}
	deriving (Eq, Show)

data Circle = Circle {origin::Point, radius :: Double}
  deriving (Eq, Show)

setRadius :: Double -> Circle -> Circle
setRadius r c = c{radius=r}