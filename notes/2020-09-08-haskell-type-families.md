-- tag note haskell ghc-extension GADTs
-- title Some haskell types, GHC rewrite rules
-- date 2020-09-02
-- source http://dev.stephendiehl.com/hask/#quantification
          https://wiki.haskell.org/GHC/Type_families
          https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families
;;
# Some haskell types


## TypeFamilies
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

## Existentially quantified types
```haskell
{-# LANGUAGE ExistentialQuantification #-}

```

## Scoped Type variables
```haskell
{-# LANGUAGE ScopedTypeVariables #-}

```

## RankNTypes
```haskell
{-# LANGUAGE ScopedTypeVariables #-}

```
