-- tag note ski-combinator combinatory-logic derive
-- title SKI combinators
-- date 2020-09-04
-- source https://www.youtube.com/watch?v=FC6kl_kLFEo
          https://en.wikipedia.org/wiki/SKI_combinator_calculus
;;
# haskell Deriving and ski combinator

## GHC Extensions for deriving
Whether a type can derive a typeclass is somewhat unintuitive. The rule of thumbs, haskell take the most conservative dicision. If there is ambiguity about the deriving it will not be able to derive.

```haskell
data T0 f a = MkT0 a deriving Eq
data T1 f a = MkT1 (f a) deriving Eq
data T2 f a = MkT2 (f (f a)) deriving Eq
```

First two cases are able to be derived directly. However, the third one can lead to unterminated instances, so by default Haskell rejects it.

##### {-# LANGUAGE StandaloneDeriving #-}
```haskell
{-# StandaloneDeriving #-}
data Foo a = Bar a | Baz String deriving instance Eq a => Eq (Foo a)
```
This extension allows you to derive a typeclass based on a specific typeclass instance.

#### {-# LANGUAGE Derive* #-}

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveApplicative #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
```

Derive the most obvious instance respectively.


#### {-# LANGUAGE DeriveVia #-}
Derive anything
```haskell
{-# LANGUAGE DeriveVia #-}
```

## SKI Combinator

All lambda calculus can be encoded with three basic combinator s k i.

```haskell
-------------------------------------------------------------
-- Define some SKI combinators.
-------------------------------------------------------------
s :: (env -> a -> b) -> (env -> a) -> env -> b
s x y z = (x z) (y z)

i :: a -> a
i a = a

k :: a -> b -> a
k = const
```

Interestingly, `S` combinator is actually an instance of applicative for reader.
