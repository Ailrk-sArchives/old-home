-- tag note haskell ghc-extension GADTs
-- title Some haskell types, GHC rewrite rules
-- date 2020-09-02
-- source https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
          https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts
;;
# Some haskell types

## Some GHC type extensions
##### Generalized Abstract Data Types
It's a way to help you explicitly write down the type of constructors.

This is an example calculator using GADTs and phantom type
```haskell
{-# LANGUAGE GADTs #-}
data Expr a where
    I :: Int -> Expr Int
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
```

This allows you to ensure `Add` can only take two Int expressions, thus avoid the problem of adding an Int to a Bool.

Here is a comparision of declaring maybe types with and without GADT
```haskell
data Maybe a = Just a | Nothing
{-# LANGUAGE GADTs #-}
data Maybe a where
    Nothing :: Maybe
    Just :: a -> Maybe a
```

Another example for list type.
```haskell
data List a = Nil | Cons a (List a)
{-# LANGUAGE GADTs #-}
data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
```

Using GADTs to declare safe list. This is only possible with GADTs. If you use normal data declaration, you will have no way to specify that b in the result of Cons is a NonEmpty rather than being fully polymorphic.

```haskell
{-# LANGUAGE GADTs, EmptyDataDecls #-}
data Empty
data NonEmpty

data SafeList a b where
    Nil :: SafeList a Empty
    Cons :: a -> SafeList a b -> SafeList a NonEmpty

-- It doesn't accept Nil
safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x
```

You can also use it with kind signature

```haskell
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
data NotSafe
data Safe

data MarkedList :: * -> * -> * where
    Nil :: MarkedList t NotSafe
    Cons :: a -> MarkedList a b -> MarkedList a c

safeHead :: MarkedList a Safe -> a
safeHead (Cons x _) = x

-- since the marker will always be not safe, this
-- function will never produce anything that can be consumed
-- by safeHead.
silly :: Bool -> MarkedList () NotSafe
silly False = Nil
silly True = Cons () Nil
```

#### TypeFamilies
It's associated type in rust.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

class A a b where
    type SumType a b
    plus a -> b -> SumType a b

instance Add Integer Integer where
    type SumType Integer Integer = Integer
    plus x y = x + y
```

#### Existentially quantified types
```haskell
{-# LANGUAGE ExistentialQuantification #-}

```

#### Scoped Type variables
```haskell
{-# LANGUAGE ScopedTypeVariables #-}

```

#### RankNTypes
```haskell
{-# LANGUAGE ScopedTypeVariables #-}

```
