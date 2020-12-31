-- tag note haskell ghc-extension GADTs
-- title Some haskell types, GHC rewrite rules
-- date 2020-09-02
-- source http://dev.stephendiehl.com/hask/#quantification
          https://wiki.haskell.org/GHC/Type_families
          https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families
          https://stackoverflow.com/questions/15589556/why-are-difference-lists-not-an-instance-of-foldable/15593349#15593349
;;
# Some haskell types

## MultiParm Typeclasses and FunctionalDependencies
This example make Instance of Convertible for Int no longer unique, so it's hard to do type inference since ghc don't know you want The instance for (Int, Integer) for (Int, Char).
```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Char

class Convertible a b where
    convert :: a -> b

instance Convertible Int Integer where
    convert = toInteger

instance Convertible Int Char where
    convert = chr
```

To avoid having ambiguous instance in multi parameter typeclass, you can use functional dependencies.

```haskell
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Convertible a b | a -> b where
    convert :: a -> b

instance Convertible Int Integer where
    convert = toInteger
```
This says for typeclass Convertible, a uniquely define b. So it's illegal to make another instance for (Int, a).

This is just for better type inference experience, technically you can annotate types for everything, but that's pretty lame.

## TypeFamilies
Write type level functions. You feed the type function with a type and it will generate a new type. It's also known as indexed type in literatures of dependent type.

There are two types of type families: type family and type synonym families. Type families is type level functions, and data families is assiciate type.

#### Type synonym families
Example of type families (unassociate form):
```haskell
-- A type level function
type family Rep a
-- Specializing types
type instance Rep Int = Char
type instance Rep Char = Int

class Convertible a where
    convert :: a -> Rep a

class Convertible Int where
    convert = chr

class Convertible Char where
    convert = ord
```

(associate form):

```haskell
{-# LANGUAGE TypeFamilies #-}
class Convertible a where
    type Rep a
    convert a -> Rep a

class instance Convertible Int where
    type Rep Int = Char
    convert chr

class instance Convertible Char where
    type Rep Char = Int
    convert ord
```

Note that the type class we need multi parameter type class and functional dependencies can now be described much more concisely.

Associate form and unassociate form have the same power of expressiveness.

Another example of type synonym families.
```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- SumType a b is an associate type you provide when you
-- are implementing an intance.
class A a b where
    type SumType a b
    plus a -> b -> SumType a b

-- So here you're saying SumType Int Int is actually just Int.
instance Add Integer Integer where
    type SumType Integer Integer = Integer
    plus x y = x + y
```

#### Data families
It allows us to create new type parameterized data constructors. It's like allowing you to provide adhoc implementation for different data constructors.

```haskell
{-# LANGUAGE TypeFamilies #-}

import qualified Data.Vector.Unboxed as V

data family Array a
data instance Array Int = IArray (V.Vector Int)
data instance Array Bool = BArray (V.Vector Bool)
data instance Array (a, b) = PArray (Array a, Array b)
data instance Array (Maybe a) = MArray (V.Vector Bool) (Array a)

class IArray a where
    index :: Array a -> Int -> a

class IArray Int where
    index (IArray xs) i = xs V.! i

class IArray Bool where
    index (IArray xs) i = xs V.! i

class (IArray a, IArray b) => IArray (a, b) where
    index (PArray xs ys) i = (index xs i, index ys i)

instance (IArray a) => IArray (Maybe a) where
    index (MArray bm xs) i =
        case bm V.! i of
            True -> Nothing
            False -> Just $ index xs i
```

## Universal quantification

Universal quantification encodes parametric polymorphism in haskell.

```haskell
{-# LANGUAGE ExplicitForAll #-}
ex1 :: forall a. [a]
ex1 = []

map' :: forall a b. (a -> b) -> [a] -> [b]
map' f = foldr ((:). f) []
```

Normally it can be omitted, but if you use rank n type you need it to specify nested quantification.



## Existentially quantified types
Using existential type to make heterogeneous list.

```haskell
{-# LANGUAGE ExistentialQuantification #-}
-- Here we know s must have a show instance.
data ShowBox = forall a. Show s => SB s deriving Show

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs
```

## Scoped Type variables
Allows free type variables to be resued in  the scope of a function
```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
mkpair :: for all a b. a -> b -> (a, b)
mkpair aa bb = (ida aa, bb)
    where
        ida :: b -> b -- use b defined at the top.
        ida = id

sizeOfPtr :: Ptr a -> Int
sizeOfPtr = sizeOf . (undefined :: Ptr a -> a)
```

## RankNTypes
N is the number of forall whidch are nested.

```haskell
{-# LANGUAGE RankNTypes #-}
-- repesent a list as an opaque value that knows how to
-- execute a foldr over a list.
newtype ChurchList a = ChurchList
  { runList :: forall r. (a -> r -> r) -> r -> r }

fromList :: [a] -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs

toList ChurchList a -> [a]
toLst xs = runList xs (:) []

cons :: a -> ChurchList a -> ChurchList a
cons x xs = ChurchList $ \k z  -> k x (runList xs k z)

append :: ChurchList a -> ChurchList a -> ChurchList a
append xs ys = ChurchList $ \k z -> runtime xs k (runList ys k z)

nil = ChurchList $ \k z -> z

singleton x = ChurchList $ \k z -> k x z
snoc xs x = ChurchList $ \k z -> runList xs k (k x z)
```
