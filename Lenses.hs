module Lenses where

data Point = Point Double Double
	deriving (Eq, Show)

data Circle = Circle Point Double
  deriving (Eq, Show)

radius :: Circle -> Double
radius (Circle _ r) = r

setRadius :: Double -> Circle -> Circle
setRadius r (Circle p _) = Circle p r