{-# language RecordWildCards #-}

module Church where

import Prelude hiding (foldr)
import Control.Applicative
import Text.Printf

------------------------------------------------------------------------
-- Introducing algebras on lists

-- We start with foldr from Prelude (non optimised)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- foldr is described as:

-- foldr, applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left.

-- An alternative perspective is:

-- Given a way to reduce a partially reduced cons, and reduce a nil,
-- foldr reduces the list.

-- The structure formed by grouping the first two arguments is an
-- F-algebra (an explanation of this is beyond scope here, but it is
-- useful to have a name).

data LAlg a r = LAlg {cons :: a -> r -> r, nil :: r}

-- Now we can modify foldr to take the algebra.

fold :: LAlg a b -> [a] -> b
fold   LAlg{..} []     = nil
fold a@LAlg{..} (x:xs) = cons x (fold a xs)

-- An example algebra

testLAlg :: LAlg Int Int
testLAlg = LAlg (+) 0

-- and an application

test1 :: Int
test1 = fold testLAlg [1,2,3]

-- But what if we partially apply fold to a list?

testList1 :: LAlg Int b -> b
testList1 = flip fold [1,2,3]

-- We now have a structure which represents the original list in a
-- different form. We can say that we are representing the list as a
-- fold. With some artistic licence, this is the Church encoding of
-- lists.

-- We can also create the same list in this form directly

testList2 :: LAlg Int b -> b
testList2 LAlg{..} = cons 1 $ cons 2 $ cons 3 nil

-- and we can reduce this list using the same algebra as above.

test2 :: Int
test2 = testList2 testLAlg

-- Note that we have not used the fold function or Haskell list
-- constructors at all here!



------------------------------------------------------------------------
-- Expressions

-- We now turn our attention to expressions of a simple arithmetic
-- language. We could represent our language as:

data ExprD
  = Lit Double
  | Add ExprD ExprD
  | Mul ExprD ExprD
  | Var String

-- and we could write a fold for expressions as above. But let's opt
-- for the Church encoding and start with the type for an algebra.

data Alg c r = Alg
  { lit :: Double -> r
  , add :: c -> c -> r
  , mul :: c -> c -> r
  , var :: String -> r
  }

-- This algebra is slightly different from the list algebra we used
-- above. Firstly, it is not parameterised by an element type as it is
-- not a container like a list. Secondly, we have chosen to use
-- separate type parameters for the _carrier_ and the _result_. The
-- reason for this will become clear later.

-- An F-algebra has the same type for the carrier and result, so we
-- can define a type synonym.

type FAlg r = Alg r r

-- An expression then, is a function from an F-algebra to a result.

type Expr r = FAlg r -> r

-- Here is a test expression.

expr :: Expr r
expr Alg{..} = var "x" `add` ((lit 1 `add` lit (-1)) `mul` var "y")



------------------------------------------------------------------------
-- Pretty printing

-- We can easily construct an algebra to pretty print expressions.

prettyAlg :: FAlg String
prettyAlg = Alg
  { lit = show
  , add = printf "(%s + %s)"
  , mul = printf "(%s * %s)"
  , var = id
  }

-- and test it on our expression

testPretty :: String
testPretty = expr prettyAlg



------------------------------------------------------------------------
-- Evaluation

-- Evaluation of an expression may fail as it may contain unbound
-- variables.

type Result = Maybe Double

-- We start with an algebra for partial evaluation which fails on any
-- variable. This will be useful in its own right later.

pevalAlg :: FAlg Result
pevalAlg = Alg
  { lit = pure
  , add = liftA2 (+)
  , mul = liftA2 (*)
  , var = const empty
  }

-- To evaluate variables, we need an environment mapping variables
-- to values.

type Env = [(String, Double)]

-- As the environment is static, we can pass it in when creating the
-- evaluation algebra. The evaluation algebra extends pevalAlg adding
-- support for variables.

evalAlg :: Env -> FAlg Result
evalAlg env = pevalAlg
  { var = flip lookup env
  }

-- Now we can test evaluation

testEval :: Result
testEval = expr $ evalAlg [("x", 7), ("y", 4)]

-- Note that even though the value of y does not affect the result,
-- we are still required to provide a value for evaluation to be
-- successful. We will address this later.



------------------------------------------------------------------------
-- Composing algebras in series

-- An algebra that "returns" an expression can be composed with
-- another algebra. The simplest way to represent this is as an
-- algebra transformer.

-- flipAlg flips arguments to operators then calls the given algebra.

flipAlg :: FAlg a -> FAlg a
flipAlg alg@Alg{..} = alg
  { add = flip add
  , mul = flip mul
  }

testFlip :: String
testFlip = expr $ flipAlg prettyAlg



------------------------------------------------------------------------
-- Profunctors

-- A profuntor is cofuntoral in its first argument and functoral in
-- its second. If that description doesn't help, let's just focus on
-- what we want to achieve. We want to be able to map either the
-- carrier or result of an algebra. To map the result we need a
-- function r -> r'. This is just fmap in Functor. To map the carrier,
-- we need a function c' -> c. Note the direction here is reversed.

class Profunctor p where
  lmap  :: (c -> a) -> p a b -> p c b
  lmap h = dimap h id
  rmap  :: (b -> d) -> p a b -> p a d
  rmap f = dimap id f
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d

instance Profunctor Alg where
  dimap h f Alg{..} = Alg
    { lit = f . lit
    , add = \x y -> f $ add (h x) (h y)
    , mul = \x y -> f $ mul (h x) (h y)
    , var = f . var
    }

-- As a simple example, we can now bracket each element when pretty
-- printing.

bracket :: String -> String
bracket = printf "[%s]"

testRmap :: String
testRmap = expr $ rmap bracket prettyAlg

-- Alternatively, we can bracket each child. Note that no brackets
-- appear at the top level.

testLmap :: String
testLmap = expr $ lmap bracket prettyAlg

-- We'll see more compelling uses for the profunctor instance below.



------------------------------------------------------------------------
-- Composing algebras in parallel

-- Algebras with the same carrier can be run in parallel.

pairAlg :: Alg c a -> Alg c b -> Alg c (a, b)
pairAlg a b = Alg
  { lit = \n   -> (lit a n, lit b n)
  , add = \x y -> (add a x y, add b x y)
  , mul = \x y -> (mul a x y, mul b x y)
  , var = \i   -> (var a i, var b i)
}

-- If we want to do evaluation and pretty printing in parallel, we
-- have to lmap each of the algebras to pull their carriers from a
-- pair carrier, then pair the algebras together.

testPair :: (String, Result)
testPair = expr $ pairAlg (lmap fst prettyAlg) (lmap snd $ evalAlg [("x", 7), ("y", 4)])



------------------------------------------------------------------------
-- Optimisation

-- Our test expression has redundant clauses which we can eliminate.

-- Firstly, we extend our pevalAlg to _short circuit_ multiplications
-- by zero to zero.

scpevalAlg :: FAlg Result
scpevalAlg = pevalAlg{mul = mul'}
  where
    mul' (Just 0) _        = pure 0
    mul' _        (Just 0) = pure 0
    mul' x        y        = liftA2 (*) x y

-- Next, we define an algebra to perform _identity elimination_. This
-- algebra leaves lits and vars alone, but removes unnecessary adds
-- and muls. To know if an operation is unnecessary, it needs to have
-- each carrier value annotated with an evaluation result. If that
-- result is statically known to be identity for the operation, the
-- operation can be optimised away.

ieAlg :: FAlg a -> Alg (a, Result) a
ieAlg alg@Alg{..} = alg{add = add', mul = mul'}
  where
    add' (_, Just 0) (y, _)      = y
    add' (x, _)      (_, Just 0) = x
    add' (x, _)      (y, _)      = add x y
    mul' (_, Just 1) (y, _)      = y
    mul' (x, _)      (_, Just 1) = x
    mul' (x, _)      (y, _)      = mul x y

-- Lastly, we need constant folding - if a node can be statically
-- evaluated, it is replaced by a literal of that value. This is not
-- an algebra, but a function mapped over the result of an appropriate
-- algebra.

cf :: FAlg a -> (a, Result) -> (a, Result)
cf Alg{..} (_, Just v) = (lit v, Just v)
cf _       x           = x

-- Now we tie all the pieces together.

optiAlg :: FAlg a -> FAlg (a, Result)
optiAlg alg = cf alg `rmap` pairAlg (ieAlg alg) (snd `lmap` scpevalAlg)

-- Our optimisations successfully eliminate all unnecessary clauses.

testPrettyOpti :: String
testPrettyOpti = fst $ expr $ optiAlg prettyAlg

-- And we can evaluate the optimised expression without needing a value for y.

testEvalOpti :: Result
testEvalOpti = fst $ expr $ optiAlg $ evalAlg [("x", 7)]
