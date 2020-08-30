-- tag note haskell ghc-extension safe-haskell rewrite-rules typeclass type-system
-- title Some GHC extensions
-- date 2020-08-27
-- source https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/deriving.html
          https://downloads.haskell.org/~ghc/8.2.1/docs/html/users_guide/glasgow_exts.html#tuple-sections
          https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf
;;
# GHC Extensions.


## Sweet GHC extensions (sugars)
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

## GHC extension for Safe haskell
##### What's the problem
Haskell trys to separate pure and effectul functions through IO monad, but there are several loop holes in the type system that cn make impure functions in the pure section. For instance `unsafePerformIO :: IO a -> a` can be a partial function and throw runtime exceptions. You want to avoid unsafe functions as much as possible, unless they are absolutely necessary, and `Safe Haskell` extension helps you to enforce this good style.

##### Safe haskell
Safe haskell is an extension that allows unsafe code be securely added into a code base by restricting features the code is allowed to use. It's like the `unsafe` block in rust. When you use stuffs like `unsafePerformIO` you can mark it as unsafe so it will be properly contained.

Safe hasekll ensure strict type safety. By default your haskell code can throw random exceptions, but with Safe haskell your safe code must be totally safe, and any code might throw exceptions should be contained in the unsafe sandbox.

A fact about safe haskell, it's an extension, but it runs in the background for each compilation by default. GHC use Safe haskell extension to tract the type violations of modules so it can infer the safety.


##### examples
This little module enabled the Safe extension. We want to ensure that the module exports codes that an untrusted code can import but cannot abuse. We are making a  plugin system that plugin authors could potentially be malicious third parties. With safe haskell we can either constraint that all plugins should be a pure function or to a restricted version of IO Momad (you can't perform certain IO Actions).

In the example below, we expose a RIO, which is the only IO monad that a plugin author can access, which voids them from executing arbitrary malicious code.

```haskell
{-# Language Safe #-}
module RIO (RIO(), runRIO, rioReadFile, rioWriteFile) where

newtype RIO a = Unsafe RIO { runRIO :: IO a}

instance Monad RIO where
  return = unsafeRIO . return
  (UnsafeRIO m) >>= k = UnsafeRI $ m >= runRIO . k

pathOK :: FilePath -> IO Bool
pathOK file = erro "Unimplemented"

rioReadFile :: FilePath -> String -> RIO ()
rioReadFile file = Unsafe RIO $ do
  ok <- pathOK file
  if ok then readFile file else return ""

rioWriteFile :: FilePath -> String -> RIO ()
rioWriteFile file contents = UnsafeIO $ do
  ok <- pathOK file
  if ok then writeFile file contents else return ()
```

For instance you are the plugin author, you will use the RIO like this:

```haskell
{-# LANGUAGE Safe #-}

runMe :: RIO ()
runMe = ...
```

This is only possible with safe hasekll. Without IO:
1. The author can access IO and write arbitrary IO actions anyway.
2. Besides that, the untrusted plugin author have no access to the `UnsafeIO` constructor; if you use template haskell you will still be able to access it, even it is not export at all.
3. There will be no restriction on what packages the plugin author can import. In that case the plugin author can just pick arbitrary library with some IO operation exposed and exploit that.


##### What is a safe language?
Under Safe Haskell extension you wirte safe in a safe dialect of haskell. A safe language has several properties: `Referential transparency`, `Module boundary control`, `Semantic consistency`, and `Strict subset`. (buzz words in large).

###### Referential transparency
Any pure function is guaranteed to be pure. So all evaluation should be deterministic and won't cause any side effects. Exceptions are banned in this senario, so functions like `take` needs to take care about edge cases.

###### Module boundary control
Not everything are exported. Only things explicited stated on the export statement will be accessible by unsafe module.

###### Semantic consistency
Importing a Safe module will not chagne the meaning of existing code that doesn't depend on that module. So you should expect your normal code works as usual even you import some safe code.

###### Strict subset
The safe language is a strict subset of haskell. It's an implementation goal and basically everthing you expect to work should work.


##### What are the restrictions under safe haskell?
You can't use
1. `TemplateHaskell` since it can be used to gain access to constructors and adt that are not exported by a module.
2. `ForeignFunctionInterface` FFI that iports function with non IO type is not allowed. (inherently unsafe code).
3. `GenralisedNewtypeDeriving` It's possible to have self overlapping instances.
4. `GHC.Generics`


##### {-# LANGUAGE Safe #-}
What does this actually do? This restruct the features of haskell that can be used to be a safe subet. Under the module with `(-# LANGUAGE Safe #-)` you can only rwrite safe dialect of haskell that we discussed above. Any module with `Safe` extension turned on will also restrict modules that can be imported by untrusted modules.

Trusted modules will be compiled with `{-# LANGUAGE Safe #-}`, and stuffs can be imported and exported freely among them.

```haskell
import safe qualified Network.Socket as NS
```

##### {-# LANGUAGE UnSafe #-}
If a module is marked as unsafe, it cannot by imported by safe code.

##### {-# LANGUAGE TrustWorthy #-}
It gives a module the permission of Safe module in terms of importing and exporting, but it doesn't actually apply any real restriction to functionalities been used. In safe haskell it's like those cowboy move you can put if you're confident about what you're doing.

## GHC extension for ffi


## GHC extension for class and instances declaration

#### Inferred contexts
a

When typeclass was first introduced it was considered an experimental feature of haskell, so initally it was added in a conservative manner. As time passed people realize there are a lot of space to extend the funtionality of typeclass, and these enhancements comes as language extension. The [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf) has a collections of examples for the rationale of different extensions.

##### {-# LANGUAGE MultiParamTypeClasses #-}
Literaly allows multiple parameters when declaring type classes. Some typeclass is define not only on one type, but on a tuple of types. By default haskell cannopt express this tupled type class. The most famous example is to define `StateMonad`.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
class Monad m => StateMonad m s where
    getS :: m s
    putS :: m -> m ()
```

The state monad is actually defined in terms of two parameters, both the state it self and the value the state return.
zz
In concurrent haskell, you have funtions like this:
```haskell
newMutVar :: a -> IO (MutVar a)
getMutVar:: MutVar -> IO a
putMutVar:: MutVar a -> a -> IO ()
```

And for ST moand you have
```haskell
newST :: a -> ST s (MutVar s a)
getST :: MutVar s a -> ST s a
putST :: MutVar s a -> a -> ST s ()
```


There are some commonalities between these group of funtions. We cannot abstract the commonalities into a typeclass in default haskell, but with multi parameter typeclass we can write

```haskel
class Monad m -> VarMonad m v where
    new :: a -> m (v a)
    get :: v a -> m a
    put :: v a -> a -> m ()
```

On extra benefit is that you can also add typeclass constraints for each parameters separately for any specific instance, which is not possible without multi parameter typeclass.

##### {-# LANGUAGE FunctionalDependencies #-}
`a | a -> b` reads as a uniquely determines b.

```haskell
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- type of c depends on the type of a and b
class Add a b c | a b -> c where
  plus :: a -> b -> c

-- This is correct
instance Add Integer Integer Integer where
  plus x y = x + y

-- This is a type error X
instance Add Integer Integer Double where
  plus x y = fromIntegral x + fromIntegral y
```

##### {-# LANGUAGE FlexibleContexts #-}
Normally the assertions in the context of the instance declaration must be of the form `C a`.

With `flexible contexts`  this rule is relaxed to that an instance declaration can have

```haskell
{-# LANGUAGE FlexibleContexts #-}

```


## GHC Extensions for deriving
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

### GHC Type extensions
##### {-# LANGUAGE GADTs #-}

#### {-# LANGUAGE TypeFamilies #-}

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

#### {-# LANGUAGE RankNTypes#-}


## GHC Rewrite Rules

####

## Conclusion

To some extension GHC extension is haskell itself, so besides those core feataures like higher order function, higher kinded types, most of new developments are delivered in the form of language extensions. So far most materials I read merely introduce extensions, or just briefly mention stuffs like `OverloadedStrings`. After you finish those books and look into some real code bases, you get devastated by 20 languages extensions and bunch of imports in every files. I fill there is a descrepency between haskell teaching materials and how people really use haskell to write program.
