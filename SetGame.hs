module SetGame where

import Control.Applicative
import Data.List
import System.Random
import Data.Ord

data Colour = Red | Purple | Green       deriving (Eq, Ord, Enum, Bounded, Show)
data Shape  = Diamond | Squiggle | Oval  deriving (Eq, Ord, Enum, Bounded, Show)
data Fill   = Solid | Open | Stripe      deriving (Eq, Ord, Enum, Bounded, Show)
data Number = One | Two | Three          deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card
  { colour :: Colour
  , shape  :: Shape
  , fill   :: Fill
  , number :: Number
  } deriving (Eq, Ord)

instance Show Card where
  show (Card c s f n) = unwords [show n, show c, show f, show s] 

deck :: [Card]
deck = Card <$> allEnum <*> allEnum <*> allEnum <*> allEnum

allEnum :: (Enum a, Bounded a) => [a]
allEnum = [minBound .. maxBound]

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g = map snd . sortBy (comparing fst) . zip (randoms g :: [Double])

isSet :: Card -> Card -> Card -> Bool
isSet card1 card2 card3 = and [ good colour
                              , good shape
                              , good fill
                              , good number
                              ]
 where
   good f = isGood f [card1, card2, card3]  

isGood :: Eq a => (Card -> a) -> [Card]-> Bool
isGood f cards = let uniq = length . nub $ map f cards 
                  in uniq == 1 || uniq == length cards



main :: IO()
main = do
    g <- newStdGen
    mapM_ print (take 10 (shuffle g deck))
