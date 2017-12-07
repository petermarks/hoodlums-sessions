{-# language FlexibleInstances #-}

module RegExVM where

class Sym e where
  char :: Char -> e -> e
  split :: e -> e -> e
  match :: e

type Prog = String -> Bool

instance Sym Prog where
  char _ _ [] = False
  char c cont (x:xs)
    | c == x = cont xs
    | otherwise = False
  split cont1 cont2 xs = cont1 xs || cont2 xs 
  match = null
  
prog :: Prog -> String -> Bool
prog = id

data Outcome = Match | Fail | Cont StreamingProg

type StreamingProg = Maybe Char -> Outcome

instance Sym StreamingProg where
  char _ _ Nothing = Fail
  char c cont (Just x)
    | c == x = Cont cont
    | otherwise = Fail
  split cont1 cont2 x = case (cont1 x, cont2 x) of
    (Match, _) -> Match
    (_, Match) -> Match
    (Fail, Fail) -> Fail
    (Cont c1, Cont c2) -> Cont $ split c1 c2
    (Cont c1, Fail) -> Cont c1
    (Fail, Cont c2) -> Cont c2
  match (Just _) = Fail
  match Nothing  = Match

stream :: StreamingProg -> String -> Bool
stream p [] = case p Nothing of
  Match -> True
  _ -> False
stream p (x:xs) = case p (Just x) of
  Cont cont -> stream cont xs
  _ -> False

example :: Sym e => e
example = i0
  where
    i0 = char 'a' i1
    i1 = split i0 i2
    i2 = char 'b' i3
    i3 = split i2 i4
    i4 = match

ch :: Sym e => Char -> e -> e
ch = char

(&&&) :: Sym e => (e -> e) -> (e -> e) -> e -> e
l &&& r = l . r

(|||) :: Sym e => (e -> e) -> (e -> e) -> e -> e
l ||| r = \c -> split (l c) (r c)

q, m, p :: Sym e => (e -> e) -> e -> e
q e c = split (e c) c
m e c = let e' = split (e e') c in e'
p e c = let e' = e $ split e' c in e'

example' :: Sym e => e
example' = p (ch 'a' ) &&& p (ch 'b') $ match