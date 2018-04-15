module Lib
    ( Backspace(..)
    , Simple(..)
    , areEqual
    , jumpSpecial
    ) where

import qualified Data.Char as C
import qualified Data.Vector as V

-- | Invariants:
--
-- @
-- switchCase . switchCase = id
-- switchCase b == c   implies   b == switchCase c
-- @
class Eq b => Backspace b where
    backspace :: b
    capsLock  :: b
    switchCase :: b -> b

instance Backspace Char where
    backspace = '#'
    capsLock  = '@'
    switchCase c = if C.isUpper c then C.toLower c else C.toUpper c

data Simple = Upper | Lower | BS | CapsLock deriving (Eq, Ord, Enum, Bounded, Show)

instance Backspace Simple where
    backspace = BS
    capsLock  = CapsLock
    switchCase Upper = Lower
    switchCase Lower = Upper
    switchCase x     = x

data State = State
    { equalSameCase :: !Bool
    , equalFlipCase :: !Bool
    } deriving (Eq, Show)

areEqual :: Backspace b => V.Vector b -> V.Vector b -> Bool
areEqual vl vr = go (State True True) (V.length vl - 1) (V.length vr - 1)
    where
        go !state ixl ixr = test state (jumpSpecial vl ixl) (jumpSpecial vr ixr)

        test state (-1, _) (-1, _) = equalSameCase state
        test _ (-1, _) _ = False
        test _ _ (-1, _) = False
        test state (ixl, capsl) (ixr, capsr) =
            let symboll = vl V.! ixl
                symbolr = vr V.! ixr
                (sc, fc) | capsl == capsr = (equalSameCase state, equalFlipCase state)
                         | otherwise      = (equalFlipCase state, equalSameCase state)
                state' = State
                    { equalSameCase = sc && symboll == symbolr
                    , equalFlipCase = fc && symboll == switchCase symbolr }
            in if state' == State False False
                then False
                else go state' (ixl - 1) (ixr - 1)

jumpSpecial ::
       Backspace b
    => V.Vector b
    -> Int         -- ^ Index to start from
    -> (Int, Bool) -- ^ (index of next non-special symbol (or -1 if none), True if odd number of caps locks were found)
jumpSpecial v = go 0 False
    where
        go !bs !caps !ix | ix < 0 = (-1, caps)
        go bs caps ix =
            let x = v V.! ix
            in case (x == backspace, x == capsLock, bs) of
                (True, _, _) -> go (bs+1) caps       (ix-1)
                (_, True, _) -> go bs     (not caps) (ix-1)
                (_, _, 0)    -> (ix, caps)
                _            -> go (bs-1) caps       (ix-1)
