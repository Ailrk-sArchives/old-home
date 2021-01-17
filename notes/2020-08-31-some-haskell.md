-- tag note haskell
-- title Some haskell
-- date 2020-08-31
-- source http://dev.stephendiehl.com/hask/#side-effects
          https://hackage.haskell.org/package/unification-fd-0.10.0.1/docs/Data-Functor-Fixedpoint.html#:~:text=Fix%20f%20is%20a%20fix,for%20all%20the%20yfoo%20functions.
          http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
;;
# Some haskell

## Bottom ⊥
A single value that inhabits all types. If bottom is evaluated, there is something wrong with the expression.

#### Some example of getting bottom evaluated.

##### undefined / error
Simplest way of getting bottom. `undefined` helps you to do type check without writing the implementation, and `error` just make a runtime error.

```haskell
f :: a -> IO a
f = undefined
```

```haskell
g :: Maybe a
g = error "not possible"
```

##### Infinite loop
If an expression never hault, it will never evaluate to a actual value.

```haskell
f :: a
f = let x = x in x
```

##### unexthaustive pattern

Imagine this partial pattern matching:

```haskell
data F = A | B
case x of
  A -> ()
```

When GHC compile it, it will automatically complete all patterns, and return bottom for unspecified patterns.

```haskell
case x of
  A -> ()
  B -> patError "soem error messages blabla"
```

#### Avoid bottoms

Easiest way is to wrap stuffs into `Maybe`. Replace partial case with Nothing.

## Monomorphism restriction

If an expression is lacking type signature, the type checker will fill the free type variables with the most specific type.

```hasekll
-- f1 :: (Show x) => x -> String
f1 x = show x

-- f2 :: () -> String
f2 = \x -> show x

-- same as f1
f3 :: (Show a) => x -> String
f3 = \x -> show x

-- same as f2 () -> String
f4 = show
```

When a type is monomorphized is rather arbitrary. Why monomorphism restriction is enforced by default is explained in `HaskellReport2010`; in short, without monomorphism restriction there will be some ambiguous types.

To disable this behavior you can use `{-# NoMonomorphismRestriction #-}`

## Type holes

Similar experience with agda, but with type only. you can put an underscore whenever you are not sure what to put there, and the compiler will infer a type for you.

```haskell
-- GHC will tell you:  _ :: [a]
head' = head _

-- GHC will tell you: _ :: t -> t1 -> t
const' :: _
const' x y = x
```

If you are super lazy you can enable `{-# PartialTypeSignature #-}`, and leave holes everywhere. GHC will only warn you about type holdes but replace holes with type it inferred.

```haskell
{-# LANGUAGE PartialTypeSignature #-}

succ' :: _ => a -> a
succ' x = x + 1
```

## Reader Monad
To access shared immutable state within a monadic context.

```haskell
-- basically it's just a newtype wrapper for function :: r -> a
newtype Reader r a = Reader { runReader :: r -> a }

-- ask >>= \x -> ... where x is the environment r
ask :: Reader r r

asks :: (r -> a) -> Reader r a

local :: (r -> r) -> Reader r a -> Reader r a
```

An example of using reader monad.
```haskell
import Control.Monad.Reader

-- The environment you provides
data Context = Context {
  foo :: String
  bar :: Int
} deriving (Show)

-- read the environment with an accessor
compute1 :: Reader Context (Maybe String)
compute1 = do
  n <- asks bar
  x <- asks foo
  if n > 0 then return (Just x) else return Nothing

-- The same as above but with ask
compute2 :: Reader Context (Maybe String)
compute2 = do
  c <- ask
  let n = foo c
  let x = bar c
  if n > 0 then return (Just x) else Nothing

-- just some desugar
compute3 :: Reader Context (Maybe String)
compute3 =
  asks bar >>= \n ->
    asks foo >>= \x ->
      if n > 0 then return (Just x) else Nothing

ex1 :: Maybe String
ex1 = runReader compute1 $ MyContext "haskell" 0
```

There are lots of monads are like Reader which is essentially a function. The basic idea is you compose these functions together and describe what gonna happen if the "environment" is passed in. The real environment is passed at last will the invocation of `run...`.

## Writer Monad
To emit a lazy stream of values from a monadic context. It's really just a 2-tuple, first element is the value to write and second is the context.

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }

-- It literately just return a new tuple with the context you pass in.
tell :: w -> Writer w ()
tell w = Writer ((), w)

-- Similarly it literately get the context out.
execWriter :: Writer w a -> w
```

Note the implementation is lazy, so maybe at the end of the day you only have bunch of unevaluated thunks and not write anything. There is a strict version of writer monad in `Control.Monad.Writer.Strict`.
