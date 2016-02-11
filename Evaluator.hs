module Evaluator where

import Prelude hiding (sum)
import qualified Prelude as P
import Control.Applicative
import Control.Monad.Identity

class ExprSym e where
  litI :: Int -> e Int
  litL :: [e a] -> e [a]
  add :: e Int -> e Int -> e Int
  sum :: e [Int] -> e Int

instance ExprSym Identity where
  litI = pure
  litL = sequence
  add = liftA2 (+)
  sum = fmap P.sum

evaluate :: Identity a -> a
evaluate = runIdentity