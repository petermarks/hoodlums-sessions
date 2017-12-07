module ArrowDiagrams where

--import Algebra.Graph.Class
import Algebra.Graph.IntAdjacencyMap
import Algebra.Graph.Export.Dot
import Control.Category
import Prelude hiding ((.), id)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Coerce

data AG a b = AG
  { iam :: IntAdjacencyMap
  , inputs :: Int
  , outputs :: Int
  }
  | IDAG

instance Category AG where
  id = IDAG
  IDAG . x = coerce x
  x . IDAG = coerce x
  a . b = AG
    { iam = iam a + iam b + vertex (outputs b) * vertex (inputs a)
    , inputs = inputs b
    , outputs = outputs a
    }

single :: Int -> AG a b
single i = AG (vertex i) i i

main :: IO ()
main = 
  writeFile "example.dot" $ export defaultStyleViaShow exampleGraph


exampleGraph :: IntAdjacencyMap
exampleGraph = iam $ single 4 . single 2 . single 1