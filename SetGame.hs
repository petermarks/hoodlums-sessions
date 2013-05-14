module SetGame where

data Colour = Red | Purple | Green       deriving (Eq, Ord, Enum, Bounded, Show)
data Shape  = Diamond | Squiggle | Oval  deriving (Eq, Ord, Enum, Bounded, Show)
data Fill   = Solid | Open | Stripe      deriving (Eq, Ord, Enum, Bounded, Show)
data Number = One | Two | Three          deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card
  { colour :: Colour
  , shape  :: Shape
  , fill   :: Fill
  , number :: Number
  } deriving (Eq, Ord, Show)


deck :: [Card]
deck = [ Card c s f n | c <- allEnum, s <- allEnum, f <- allEnum, n <- allEnum]

allEnum :: (Enum a, Bounded a) => [a]
allEnum = [minBound .. maxBound]

