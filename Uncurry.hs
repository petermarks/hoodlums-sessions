{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
-- {-# language FlexibleContexts #-}
-- {-# language ScopedTypeVariables #-}
-- {-# language DataKinds #-}
-- {-# language TypeOperators #-}
-- {-# language OverlappingInstances #-}


-- At this month's Hoodlums meetup
-- (https://www.meetup.com/hoodlums/events/hrbdtnyxhbnb/), Pepe
-- (https://www.meetup.com/hoodlums/members/12783660/) challenged us to write
-- uncurryN which uncurries a curried function of any arity. We were partially
-- successful, but our solution requires an instance of Uncurry for each
-- argument type we want to support. The version presented here achieves it
-- without that overhead.

module Uncurry where

-- For some of the alternative solutions presented at the end of this file,
-- Data.Proxy and/or various combinations of the commented out language pragmas
-- above are required.

-- import Data.Proxy


-- uncurry2 is uncurry from The prelude included here to help us see a pattern. 

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a, b) = f a b


-- uncurry3 applies uncurry2 twice. The first application pairs the first two
-- arguments, the second pairs this pair with the third argument. We chose to
-- nest our tuples this way, but other arrangements are possible. A flat tuple
-- structure for uncurryN is probably not possible.

-- The type of the first uncurry2 (on the right) is specialised to:
--   uncurry2 :: (a -> b -> (c -> d)) -> (a, b) -> (c -> d)
-- (the parens around c -> d are added for clarity of explanation)

-- After applying it to the function, we have a function of type:
--   (a, b) -> c -> d

-- So the second uncurry2 (on the left) specialises to:
--   uncurry2 :: ((a, b) -> c -> d) -> ((a, b), c) -> d

uncurry3 :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurry3 = uncurry2 . uncurry2


-- uncurry4 applies uncurry2 then uncurry3. Swapping these the other way round
-- is exactly the same, but this way generalises better to uncurryN below.

uncurry4 :: (a -> b -> c -> d -> e) -> (((a, b), c), d) -> e
uncurry4 = uncurry3 . uncurry2


-- Before we can write uncurryN, we need to be able to express its type. We use
-- a closed type family following the pattern of our examples above.

type family Uncurried a b where
  Uncurried a (b -> c)  = Uncurried (a, b) c
  Uncurried a b = a -> b


-- To implement uncurryN, we tried to write:

-- class Uncurry a b where
--   uncurryN :: (a -> b) -> Uncurried a b

-- instance (Uncurry (a, b) c) => Uncurry a (b -> c) where
--   uncurryN = uncurryN . uncurry2

-- instance Uncurry a b where
--   uncurryN = id

-- The problem is that in the base case instance, the compiler doesn't know that
-- b is not a function and so Uncurried a b is a -> b.

-- The solution Pepe used was to write a specific instance for each type he
-- wanted to support, rather than this catch all instance. This works fine and
-- satisfied his requirements, but it would be good not to have this overhead.

-- We tried a few ideas including using an associated type family, which can be
-- seen in the previous version of this file, but we couldn't get this working
-- during the meetup.


-- The solution I present here follows Sergey's
-- (https://www.meetup.com/hoodlums/members/194418183/) suggestion of making the
-- output type a class parameter. I think he proposed to use functional
-- dependency to select the correct instance at the call site, but I use a
-- wrapper function instead.

uncurryN :: (Uncurried a b ~ o, Uncurry a b o) => (a -> b) -> o
uncurryN = uncurryN'

class Uncurry a b o where
  uncurryN' :: (a -> b) -> o

instance (Uncurried a (b -> c) ~ o, Uncurry (a, b) c o) => Uncurry a (b -> c) o where
  uncurryN' = uncurryN . uncurry2

instance Uncurry a b (a -> b) where
  uncurryN' = id
  
-- Many of the approaches we tried required OverlappingInstances and I'm not
-- quite sure how compiler knows that this doesn't, but hey, I'll take the win.



-- ************************************************************
-- Alternative solutions follow. These should all work given the necessary
-- pragmas and imports. You need to comment out the code above as these clash.
-- ************************************************************



-- At the meetup, we contemplated using a single argument to the type function.
-- This version does that, and also for the input type in the type class. I find
-- this presentation slightly clearer, but it is a bit further from the code we were
-- working on.

-- type family Uncurried f where
--   Uncurried (a -> b -> c)  = Uncurried ((a, b) -> c)
--   Uncurried (a -> b) = a -> b

-- uncurryN :: (Uncurried (a -> b) ~ o, Uncurry (a -> b) o) => (a -> b) -> o
-- uncurryN = uncurryN'

-- class Uncurry f o where
--   uncurryN' :: f -> o

-- instance (Uncurried (a -> b -> c) ~ o, Uncurry ((a, b) -> c) o) => Uncurry (a -> b -> c) o where
--   uncurryN' = uncurryN . uncurry2

-- instance Uncurry (a -> b) (a -> b) where
--   uncurryN' = id



-- Before I came to the solutions above, I thought that some of our difficulty
-- was coming from overlapping instances, so I tried using a technique from
-- Kwang's blog post
-- https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
-- to avoid overlapping.

-- This doesn't work for the same reason as the original attempt.

-- type family (IsFunction a) :: Bool where
--   IsFunction (a -> b)  = 'True
--   IsFunction a = 'False

-- type family Uncurried a b where
--   Uncurried a (b -> c)  = Uncurried (a, b) c
--   Uncurried a b = a -> b

-- uncurryN :: forall a b p . (IsFunction b ~ p, Uncurry p a b) => (a -> b) -> Uncurried a b
-- uncurryN = uncurryN' (Proxy :: Proxy p)

-- class Uncurry (p :: Bool) a b where
--   uncurryN' :: Proxy p -> (a -> b) -> Uncurried a b

-- instance (IsFunction c ~ p, Uncurry p (a, b) c) => Uncurry 'True a (b -> c) where
--   uncurryN' _ = uncurryN . uncurry2

-- instance Uncurry 'False a b where
--   uncurryN' _ = id



-- Once I added the output type parameter, this worked, but it turned out that
-- avoiding overlapping was not necessary. I include this version here because
-- it is an interesting technique none the less.

-- type family (IsFunction a) :: Bool where
--   IsFunction (a -> b)  = 'True
--   IsFunction a = 'False

-- type family Uncurried a b where
--   Uncurried a (b -> c)  = Uncurried (a, b) c
--   Uncurried a b = a -> b

-- uncurryN :: forall a b p o . (Uncurried a b ~ o, IsFunction b ~ p, Uncurry p a b o) => (a -> b) -> o
-- uncurryN = uncurryN' (Proxy :: Proxy p)

-- class Uncurry (p :: Bool) a b o where
--   uncurryN' :: Proxy p -> (a -> b) -> o

-- instance (Uncurried a (b -> c) ~ o, IsFunction c ~ p, Uncurry p (a, b) c o) => Uncurry 'True a (b -> c) o where
--   uncurryN' _ = uncurryN . uncurry2

-- instance Uncurry 'False a b (a -> b) where
--   uncurryN' _ = id



-- This is the same as above, but with the input type as a single parameter.

-- type family (IsFunction a) :: Bool where
--   IsFunction (a -> b)  = 'True
--   IsFunction a = 'False

-- type family Uncurried f where
--   Uncurried (a -> b -> c)  = Uncurried ((a, b) -> c)
--   Uncurried (a -> b) = a -> b

-- uncurryN :: forall a b p o . (Uncurried (a -> b) ~ o, IsFunction b ~ p, Uncurry p (a -> b) o) => (a -> b) -> o
-- uncurryN = uncurryN' (Proxy :: Proxy p)

-- class Uncurry (p :: Bool) f o where
--   uncurryN' :: Proxy p -> f -> o

-- instance (Uncurried (a -> b -> c) ~ o, IsFunction c ~ p, Uncurry p ((a, b) -> c) o) => Uncurry 'True (a -> b -> c) o where
--   uncurryN' _ = uncurryN . uncurry2

-- instance Uncurry 'False (a -> b) (a -> b) where
--   uncurryN' _ = id



-- This was actually the first version I got to work. I had gone through some
-- steps making the proxy richer, but it was adding the output type as a class
-- parameter that was the key. From this I backtracked and simplified until I
-- got to the version presented above.

-- data Sig a = Ret a | a :-> Sig a

-- infixr 5 :->

-- type family (SigOf a) :: Sig * where
--   SigOf (a -> b) = a ':-> SigOf b
--   SigOf a = 'Ret a

-- type family (Uncurried (as :: Sig *)) where
--   Uncurried (a ':-> 'Ret b) = a -> b
--   Uncurried (a ':-> b ':-> c) = Uncurried ((a, b) ':-> c)

-- uncurryN :: forall f s . (SigOf f ~ s, Uncurry s f) => f -> Uncurried s
-- uncurryN = uncurryN' (Proxy :: Proxy s)

-- class Uncurry (s :: Sig *) f where
--   uncurryN' :: Proxy s -> f -> Uncurried s

-- instance Uncurry (a ':-> 'Ret b) (a -> b) where
--   uncurryN' _ = id

-- instance (SigOf c ~ s, Uncurry ((a, b) ':-> s) ((a, b) -> c)) => Uncurry (a ':-> b ':-> s) (a -> b -> c) where
--   uncurryN' _ = uncurryN . uncurry2