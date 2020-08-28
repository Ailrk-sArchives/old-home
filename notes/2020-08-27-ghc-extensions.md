-- tag note haskell boolean-blindess evidence idioms ghc-extension
-- title Haskell Idioms
-- date 2020-08-17
-- source https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/deriving.html
          https://downloads.haskell.org/~ghc/8.2.1/docs/html/users_guide/glasgow_exts.html#tuple-sections

# GHC Extensions

#### Sweet GHC extensions (sugars)
##### {-# LANGUAGE LambdaCase #-}
Pattern match on the parameter of a lambda function directly. It saves you from nameing the parameter.

```haskell
f x :: Maybe a -> Maybe a
f = \case
    Just a -> Just a
    Nothing -> Nothing
```

##### {-# LANGUAGE OverloadedStrings #-}
Polymorphic string over `Text` and `ByteString`.

Bascially allows you to have a function `f :: ByteString -> Int`, and call it like `f "good"`. This is natural for numbers because number literals are by default polymorphic. However it doesn't comes by default for String.

##### {-# LANGUAGE RecordWildCards #-}
Allowing to use wildcard in record construction and pattern matching. This extension implies `DisambiguateRecordFields`. `..` can be used in a record like `f (C {a = 1, b, ..}) = b + c + d` where a will be updated to a and other part in the record will be the same.

The record wildcard expension will refer to the nearest enclosing variables that has the same name as some record field.

```haskell
-- spread
C {a = 1, ..} = e
-- in expression
let {a = 1; b = 2; c = 3; d = 4} in C {..}
-- the same as
let {a = 1; b = 2; c = 3; d = 4} in C {a=a, b=b, c=c, d=d};
```

##### {-# LANGUAGE TupleSections #-}
Translate a tuple like `(, "I", , , "Love, ,1335")` to `\a b c d -> (a, "I", b, c, "Love, d, 1335")`. You have all kinds of extensions to provide nicer syntax for a very specific use case...

##### {-# LANGUAGE MultiWayIf #-}
Allow a guard-like if expression

```haskell
if | guard1 -> expr1
   | ...
   | guardN -> exprN
```

This is actually equivalent to

```haskell
case () of
    _ | guard1 -> expr1
     ...
    _ | guardN -> exprN
```

#### GHC extension for class and instances declaration

###### {-# LANGUAGE FunctionalDependencies #-}


##### {-# LANGUAGE FlexibleContexts #-}
Normally the assertions in the context of the instance declaration must be of the form `C a`.

With `flexible contexts`  this rule is relaxed to that an instance declaration can have

```haskell
{-# LANGUAGE FlexibleContexts #-}

```


#### GHC Extensions for deriving
Whether a type can derive a typeclass is somewhat unintuitive. The rule of thumbs, haskell take the most conservative dicision. If there is ambiguity about the deriving it will not be able to derive.

```haskell
data T0 f a = MkT0 a deriving Eq
data T1 f a = MkT1 (f a) deriving Eq
data T2 f a = MkT2 (f (f a)) deriving Eq
```

First two cases are able to be derived directly. However, the third one can lead to unterminated instances, so by default Haskell rejects it.

##### {-# LANGUAGE StandaloneDeriving #-}
```
{-# StandaloneDeriving #-}
data Foo a = Bar a | Baz String deriving instance Eq a => Eq (Foo a)
```
This extension allows you to derive a typeclass based on a specific typeclass instance.

##### {-# GeneralizedNewtypeDeriving #-}

##### {-# LANGUAGE DerivingStrategies #-}

#### Type extensions
##### {-# LANGUAGE GADTs #-}

#### {-# LANGUAGE TypeFamilies #-}

#### {-# LANGUAGE RankNTypes#-}

