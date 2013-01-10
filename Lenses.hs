module Lenses where

data Point = Point {x :: Double, y :: Double}
	deriving (Eq, Show)

data Circle = Circle {origin::Point, radius :: Double}
  deriving (Eq, Show)

setRadius :: Double -> Circle -> Circle
setRadius r c = c{radius=r}

setOrigin :: Point -> Circle -> Circle
setOrigin p c = c{origin=p}

setX :: Double -> Point -> Point
setX v p = p{x=v}

setY :: Double -> Point -> Point
setY v p = p{y=v}