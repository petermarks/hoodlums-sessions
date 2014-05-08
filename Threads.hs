{-# language TypeSynonymInstances, FlexibleInstances, 
      GADTs, KindSignatures, ExistentialQuantification #-}

module Threads where

import Control.Monad

data Thread a
  = forall b . Step {action :: IO b, callback :: b -> Thread a}
  | Return a

instance Monad Thread where
  return = Return
  (Return v) >>= f = f v
  (Step action cont) >>= f = Step action (cont >=> f)

runThreads :: [Thread a] -> IO [a]
runThreads ts 
  | Just vs <- values = return vs
  | otherwise         = mapM f ts >>= runThreads
  where
    f :: Thread a -> IO (Thread a)
    f (Return v)         = return $ Return v
    f (Step action cont) = fmap cont action
    values           = mapM value ts
    value (Return v) = Just v
    value _          = Nothing

runThread :: Thread a -> IO a
runThread (Return v) = return v
runThread (Step action cont) = action >>= runThread . cont

data V :: * -> * where
  VString :: String -> V String
  VInt    :: Int    -> V Int

class ToV a where
  v :: a -> V a

instance ToV String where
  v = VString

instance ToV Int where
  v = VInt

out :: V String -> Thread ()
out (VString s) = Step (putStrLn s) Return

toString :: V Int -> V String
toString (VInt i) = VString $ show i

dec :: V Int -> V Int
dec (VInt i) = VInt $ i - 1

ifZero :: V Int -> Thread a -> Thread a -> Thread a
ifZero (VInt i) t f = if i == 0 then t else f

------------------------------------------------------------

test :: Thread ()
test = go (v 5)
  where
    go i = ifZero i (return ()) (out (toString i) >> go (dec i))

main :: IO ()
main = void $ runThreads [test, test]