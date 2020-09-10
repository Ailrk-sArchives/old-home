-- tag paper haskell applicative
-- title Report on `Applicative programming with effects`
-- date 2020-09-06
-- source http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
;;
# Report on `Applicative programming with effects`

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

{-
   2020-09-05
   Report on Applicative programming with effects.
   http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
-}

module Ap () where

-------------------------------------------------------------
-- Define some SKI combinators.
-------------------------------------------------------------
s :: (env -> a -> b) -> (env -> a) -> env -> b
s x y z = (x z) (y z)

i :: a -> a
i a = a

k :: a -> b -> a
k = const

{-------------------------------------------------------
-- Some common applicative functor with monad.
   These are examples implemented with monad but really
   are applicatives.
--------------------------------------------------------}

-- 1. seqence
seqIO :: [IO a] -> IO [a]
seqIO [] = return []
seqIO (x : xs) = do
  c <- x
  cs <- seqIO xs
  return $ c : cs

-- effect only
seqIO_ :: [IO a] -> IO ()
seqIO_ xs = seqIO xs >> return () -- just discard the effect

{-
2. transpose
    Primitive version
-}
transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs : xss) = zipWith (:) xs $ transpose xss

-- with zapp: "zippy application"
repeat' :: a -> [a]
repeat' x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f : fs) (x : xs) = f x : zapp fs xs
zapp _ _ = []

transpose' :: [[a]] -> [[a]]
transpose' [] = repeat' []
transpose' (xs : xss) = repeat' (:) `zapp` xs `zapp` transpose xss

{-
3. Evaluating expression with applicative like monad.
   Notice addition, + is carry over the environment, and in this case
   k combinators act like const, which is similar to pure for reader.
   s is really a <*>.
-}
data Expr v where
  Var :: v -> Expr v
  Val :: Int -> Expr Int
  Add :: Expr v -> Expr v -> Expr v

eval :: (Integral v) => Expr v -> Expr v -> Int
eval (Var x) = const $ fromIntegral x
eval (Val i) = k i
eval (Add p q) = k (+) `s` eval p `s` eval q

{-------------------------------------------------------
  -- canonical form of applicative programming
  Computations have a fixed structure, given by the pure function and
  a sequence of sub computations, given by effectful arguments.
  Think it as normal function application, but carrying extra effects.
--------------------------------------------------------}

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = pure f <*> a <*> b

-- Make applicative a functor. Similar as above.
fmap' :: Applicative f => (a -> b) -> f a -> f b
f `fmap'` m = pure f <*> m

-- use applicative style
-- note the partially applied function itself has an instance of Applicative.
seqIOA :: [IO a] -> IO [a]
seqIOA [] = pure []
seqIOA (c : cs) = pure (:) <*> c <*> sequence cs

evalA :: (Integral a) => Expr a -> Expr a -> Int
evalA (Var x) = eval (Var x)
evalA (Val i) = pure i
evalA (Add p q) = pure (+) <*> evalA p <*> evalA q

{-------------------------------------------------------
  -- Traverse data structure
--------------------------------------------------------}
-- 1. Applicative distributor
-- Pull context from list to the out most layer. This is the
-- motivation of Traversable.
--
-- Make the list part generic you get sequence
dist :: Applicative f => [f a] -> f [a]
dist [] = pure []
dist (v : vs) = pure (:) <*> v <*> dist vs

-- dist is often used with map, which is the motivation of traverse.
flakyMap :: Applicative f => (a -> f b) -> [a] -> f [b]
flakyMap f ss = dist (f <$> ss)

-- Generalize traversable to applicatives other than list.
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node l a r) = (foldMap f l) <> f a <> (foldMap f r)

instance Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Node l x r) = Node <$> (traverse f l) <*> (f x) <*> (traverse f r)

{-------------------------------------------------------
  -- Monoids are phantom applicative functors
     Every monoids induces an applicative functor
--------------------------------------------------------}
-- first we define a phantom type. Only o will exist at term level.
newtype Accy o a = Acc {acc :: o} deriving (Functor)

-- then we define the applicative for Accy
-- Note that there is no value for type a, so really no application
-- what so ever. Only thing that happens is combine the monoid part.
instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  Acc o' <*> Acc o'' = Acc $ o' <> o''

-- Acc . f :: a -> Acc o a
-- traverse (Acc . f) :: t a -> Acc o (t a)
-- But a is really nothing, so again eventually the <*> just combine
-- monoids o.
accumulate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = acc . traverse (Acc . f)

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = accumulate id

-- use traversable to make some useful functions.
-- (:[]) :: a -> [a], and [a] is a monoid.
-- So in words it traverse the entire tree, turn each element into a
-- list with the element in it, and concat all lists.
flatten :: Tree a -> [a]
flatten = accumulate (: [])

concat' :: [[a]] -> [a]
concat' = reduce

{-------------------------------------------------------
  -- Comparision with monad
  -- 1.
      ap :: Monad m => m (a -> b) -> m a -> m b is application defined
      on monad. We can define an applicative functor for every single
      monad: as long as you define the ap you get applicative for free.
  -- 2.
      Some applicative can't be a monad. There're more appicative than
      monad because it's more generic.
  -- 3.
      For a monad, (>>=) :: m a -> (a -> m b) -> m b allows the value
      returned by one computation to influence the choice of another
      (in the form of parameter, and you can use the parmeter to change
      how the resulting monad should be).
      On the other hand, applicative only sequencing effects and  keep
      the computation structure unchange.
  -- 4.
      Applicative is closed under composition, but for monads it's not
      necessarily true.
      For monad, for instance, IO . Maybe is not necessarily the same as
      IO . Maybe. Moreover, IO . Maybe might not be another monad to
      begie with.
      Because all monads are applicative, all monads can compose their
      effects as composition of applicative. However, a notable difference
      between applicative composition between applicatives and mondas is
      that for applicatives you must perform all effects, but for monad
      some effect might fail and abort the execution.
--------------------------------------------------------}
-- Monad bind allows the result of the first computation influence
-- the resulting monad.
-- In the example the result of mb determines whether mt or me is performed.
miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt me = do
  b <- mb
  if b then mt else me

-- In the case of applicative all effects will be performed. You can't choose.
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = pure cond <*> fb <*> ft <*> fe
  where
    cond b t e = if b then t else e

-- Applicative is close under composition
-- Two monads might not compose to make another monad, but since all
-- monads are applicatives, you at least get a composition of applicatives.
-- So if you have monads you can always combine their effects.
newtype Comp f g a = Comp {comp :: (f (g a))} deriving (Functor)

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  pure x = Comp $ (pure . pure) x
  Comp fs <*> Comp xs = Comp $ pure (<*>) <*> fs <*> xs

{-------------------------------------------------------
  -- Conclusion
     The first intution of applicative is application under effects. With
     applicatives you can have a function apply on a sequence of effectful
     arguments. During the process of application you combine effects
     together.

     Applicative is less powerful than Monad in the sense that it must
     perform all effects it entails. For monad, you have the entire function
     of (a -> m b) to decide what to do with a --- the result of m a, but for
     applicative pure f <*> a <*> b ... means you will perform effects of a,
     b, ... sequentially, there is no control mechanism.
--------------------------------------------------------}

-- skipped : arrow
```
