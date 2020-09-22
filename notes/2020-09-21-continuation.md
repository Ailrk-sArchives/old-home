-- tag note haskell continuation
-- title Haskell data parallelism, concurrency, and polymorphic size array
-- date 2020-09-11
-- source https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
;;
# Continuation and denotational semantics.

## Continuation
A style of programming that functions do not return values but rather pass the control into a continutation.

For instance, the callback style in nodejs. You don't return anything, but rather pass the result of the computation into the callback.

Look at some eample:
```haskell
-- normal
let r1 = map (*2) [2, 4, 8]

-- continuation passing style
let r2 = map ($ 2) [(*2), (*4), (*8)]
```
Recall `($) :: (a -> b) -> a -> b` is just function application.

Here, `($ 2) :: (a -> b) -> b` is a suspended computaion that requires us to pass another function to make it complete.

##### Another explaination
Think of expression (+ 1 (+ 2 (+ 3 4))). (+ 2 _) will be evaluated after the hole (+ 3 4) is evaluated.

A computation with hold is a continuation. Continuation is sequential in nature: one continuation is chained with another continuation.

If you manually expose the hold as an argument, we can write
```
kplust x y k = k (+ x y)
let a = kplus 2 3 (\n -> kplus 1 n id)
```

PS: Any plain value can be converted into suspened computation with `flip ($) :: a -> (a -> b) -> b` . Because if you apply a value to it the expression will become the same form of `($ 2)` , and waiting for another function that use the value to complete it's task.

#### Benefits
Continuation allows to drastically change the contro flow of the program. You can simulate early return and goto with cps; exceptions and failures can be expressed with cps (in nodejs for example you pass in a continuation and a exception handler); you can also describe coroutine with cps: suspend a computation and resume it at some later time. Continuation sometimes can be used to implement cooperative concurrency.

In haskell you can use cps to implement complicated control flow in monad.

#### Examples

##### Pythagoras
The funcion pass the value that it should return if it is not in cps style to the continutaion and hand the control over.
```haskell
add :: Int -> Int -> ((Int -> r) -> r)
add x y f = f (+ x y)

square :: Int -> ((Int -> r) -> r)
square x f = f (* x x)

[pyth](pyth) :: Int -> Int -> ((Int -> r) -> r)
pyth x y = \k ->
    square x $ \x2 ->
    square y $ \y2 ->
    add x2 x2 $ k
```

##### Thrice
```haskell
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

-- rule of thumb all return value should be converted into a continuation.
thriceCPS :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thriceCPS f x = \k ->
    f x $ \fx ->
    f fx $ \fxx ->
    f ffx $ k
```

##### ChainCPS
```haskell
chainCPS :: ((a -> r) -> r) -> (r -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f = \k -> s $ \x -> f x $ k
```

#### `Cond` monad
We are in haskell, so we want to make cps a monad!

```haskell
newtype Cont r a =  { runCont :: (a -> r) -> r }
instance Monad (Cont r) where
    return x = Cont ($ x) -- convert the value into a continuation
    s >>= f =  Cont $ \c -> runCont s $ \x -> runCont (f x) c
    -- the continuation of s is (f x)
    -- c is the continuation for the composed computation.
cont :: ((a -> r) -> r) -> Cont r a
cont = Cont
```

Now you can compose continuations.
```haskell
import Control.Monad.Trans.Cont

add :: Int -> Int -> Cont r Int
add x y = return (+ x y)

square :: Int -> Cont r Int
square x = return (* x x)

-- Now the order is sequential again.
pyth :: Int -> Int -> Cont r Int
pyth x y = do
    x2 <- square x
    y2 <- square y
    add x y
```

#### `callCC` call with current continuation
`callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a` Bring the continuation back from `Cont r a` monad.

```haskell
square :: Int -> Cont r Int
square n = callCC $ \k -> k (n ^ 2)
```
callCC let you supply a function and generate a suspend computation. What makes this different from using cont monad alone?

Note the argument k: Call it anywhere will cause the value get passed to it being made into suspend computation. The suspend computation is then inserted into the contro flow at where the callCC get called. Whatever follows the call of `callCC` will be discarded.

##### deciding when to use k
```haskell
-- Use calCC as early return.
foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    return (show $ y - 4)
```

So in the example above, if y > 20 we call k, we return from the callCC immediately with "over twenty" get pass into the continutation. Notice the value get passed into the continuation has the same notion of a return value in a non cps function, so k here bascially act likea early return. Of course, if y < 20 the last expression will get evaluated.

```haskell
-- another example
-- Here k behaves like a goto.
-- if the pred is true the rest of the code will note
-- be executed at all.
bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello"
        let s1 = show s0
        return ("Say" ++ s1)
    return (length msg)
```

```haskell
-- in this example the last line will never get executed.
quux :: Cont r Int
quux = callCC $ \k -> do
    let n = 50
    k n
    return 25   -- this will never be executed.
```

##### More about callCC
```haskell
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
```
Take a look at how it works in the same example.
The overall return type should be `Cont r Int`, not matter if x < 10 or not.
If n >= 10, calCC return 25, which just convert the value into a suspend computation.

If n < 10, callCC invoke k on n. so what is k in this case? first of all, k has type `k :: Int -> Cont r b`. k's argument is made into a suspend computation inserted at the position of callCC invocation.

```haskell
foo :: Int -> Int -> Int -> Cont r Int
foo n x y = callCC $ \k -> do
    when (n < 10) $ k x
    when (n < 20) $ k y
    return 25
```

##### More complicated control flow

```haskell
-- exception
divExcept :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcept x y handler = callCC $ \ok -> do
    err <- callCC $ \notOk -> do
        when (y == 0) $ notOk "Denominator 0"
        ok $ x `div` y
    handler err
```
