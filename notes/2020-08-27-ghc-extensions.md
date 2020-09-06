-- tag note haskell ghc-extension safe-haskell rewrite-rules typeclass type-system ffi
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

This is only possible with safe hasekll. The result:

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

## GHC extension for foreign function interface
Call foreign functions in haskell. FFI is enabled by default, so strictly speaking it's not an extension. You can control ableness with `ForeignFunctionInterface` flag.

#### Call safety
Safe ffi calls must allow foreign calls to safely call into hasekll code, this means that the gc must be able to run while foreign calls are in progress.

If you pass a foreign heap objecet reference to haskell without any constraints, the gc will be able to move the reference arround the heap. A safe ffi requries your reference is `pinned`, so the gc can no longer move it arbitrarily.

I an `unsafe` call the foreign reference passed in doesn't required to be pinned, but after ghc 8.4 gc will never happen during a unsafe foreign call, so at least the heap itself won't move during the execution of the foreign call.

#### UnliftedFFITypes
Some basic foreign types `Int#`, `Word#`, `Char#`, `Float#`, `Double#`, `Addr#`, `StablePtr# a`.

Some boxed types can be used in foreign import with some restrictions.

###### Types can be used as argument of `foreign import unsafe`
`Array#`, `SmallArray#`, `ArrayArray#`, `ByteArray#`, `MutableArray#`, `MutableSmallArray#`, `MutableArrayArray#`, `MutableByteArray#`

###### Types can be used as argument of `foreign import safe`
__Pinned__ `ByteArray#`, `MutableByteArray`.

###### Mutation
It's safe to mutate `MutableByteArray` in both `safe` and `unsafe` foreign import.

#### Example.
Simple example. It sums the first three bytes without using anything from the `Rts.h` .
```c
uint_8_t  add_triplet(uint_8_t* arr) {
    return (arr[0] + arr[1] + arr[2]);
}
```

```haskell
foregin import ccall unsafe "add_triplet"
    addTriplet :: MutableArray# RealWorld -> IO Word 8
```

Sometimes your foreign language needs to know the rts closure type. In the following example the `StgArrayBytes` is a rts construct.

```c
#include <Rts.h>
int sum_first(StgArrayBytes **bufs) {
    StgArrayBytes **bufs = (StgArrayBytes**)bufsTmp;
    int res = 0;
    for (StgWord ix = 0; ix < arr->ptrs; ix++) {
    res = res + ((int*)(bufs[ix]->payload)[0]);
    }
    return res;
}
```

```haskell
--
foregin import ccall unsafe "sum_first"
    sumFirst :: ArrayArray# -> IO CInt
```


#### Wrap around IO monad.

```haskell
newtype MIO a = MIO (IO a)
```

It's ok to replace IO type with your customized IO newtype wrapper in a foreign import. GHC will recognize the wrapper IO and accept it as an IO monad.

This is acceptable even if there is no IO monad explicitly specified in the type signature.
```haskell
foreign import "dynamic"
    baz :: (Int -> MIO Int) -> CInt -> MIO Int
```

`MIO` limits IO actions can be performed by, prevents you from calling arbitrary IO actions.

#### Explicit forall
You can quantify type variables in foregin import with explicit forall.
```haskell
{-# LANGUAGE ExplicitForAll #-}
foreign import ccall "mmap" cMmap :: forall a. CSize -> IO (Ptr a)
```

#### Memory allocation
FforFI library provides some functions to allocate memory explicitly. There are some differences between these meneory allocation methods.

###### `alloca`
Shor-term allocation. Internally it uses `MutableByteArray#`, so the allcation and deallocation are fast. It's faster then C's heap malloc/free, but slower than C's heap allocation. Because it has the lowest overheat, it's the best choice for most of the time. Ppl normally use it to marshaling data to and from FFI functions.

###### `mallocForeignPtr`
Long-term allocation that requires garbage collection. Internally using `MutableByteArray#`, so it's very similar to `alloca` but the memory is pointed by a `ForeignPtr`.

###### `Foreign.malloc / Foreign.free`
Simply a wrapper over c's `malloc/free`. It's much slower than other methods, so use it as the last resort.

###### `Foreign.Marshal.Pool`
A pool implemented with `malloc/free`.


#### Pinned Byte Array
If a byte array is pinned it cannot be moved by the garbage collector. The byte array has a stable address that can be safely requested with `byteArrayContents#`.

You can get a pinned byte array by allocating it with `newPinnedByteArray#`. Besides that, if the byte array is large (as large as 80% of a 4kb block) or if ithas been copied into a compact region, rts will pin it automatically.


## GHC extension for class and instances declaration

When typeclass was first introduced it was considered an experimental feature of haskell, so initally it was added in a conservative manner. As time passed people realize there are a lot of space to extend the funtionality of typeclass, and these enhancements comes as language extension. The [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf) has a collections of examples for the rationale of different extensions.

#### Inferred contexts

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

```haskell
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


## Conclusion

To some extension GHC extension is haskell itself, so besides those core feataures like higher order function, higher kinded types, most of new developments are delivered in the form of language extensions. So far most materials I read merely introduce extensions, or just briefly mention stuffs like `OverloadedStrings`. After you finish those books and look into some real code bases, you get devastated by 20 languages extensions and bunch of imports in every files. I fill there is a descrepency between haskell teaching materials and how people really use haskell to write program.
