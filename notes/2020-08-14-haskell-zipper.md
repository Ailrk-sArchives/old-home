-- tag note haskell data-structures zipper ST
-- title Some more haskell notes. ST zipper
-- date 2020-08-14
-- source https://en.wikibooks.org/wiki/Haskell/Zippers
          https://github.com/xmonad/xmonad
          https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell
          https://wiki.haskell.org/Monad/ST
          https://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar
          https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#DataConstructors
;;
# ST, Zipper.

## ST monad
`STRef` is a more generic form of `IORef` that allows you to have mutable reference. `ST s a` is a `state` like monadic interface helps you to use `STRef`. `runST :: (forall s. ST s a) -> a` is the function to execute a `ST` computation, and it use `rank 2 type` to enforce the mutable reference is always to each `runST`, so that the side effect can only happen inside the `runST`, and externally behave like a pure function.

#### A very cool implementation of quick sort with in place mutation in haskell
Without mutation, to write a quick sort is actually easier. Like this:
```haskell
quickSort1 :: (Ord) => [a] -> [a]
quickSort1 [] = []
quickSort1 (x:xs) = left ++ [x] ++ right
  where
    left = quickSort1 $ filter (<=x) xs
    right = quickSort1 $ filter (>x) xs
```

This version looks very neat, just doesn't perform very well. Because everything is immutable, so the runtime will create a new list in each iteration. Although the gc process is not too bad for immutable data, it is still not idea compare with a in place mutation.

With `STRef` and `ST s a` monad we can have inplace mutation while providing a pure interface. This allows us to implement algorithms that are more efficient with mutation.

##### Implementation
`STRef` and `STArray` are basical notion of dealing with mutation in haskell, and `ST s a` monad provides a monadic environment for the mutation to happen.
 Expr

###### Swap
The swap logic can just be written in the say way as you would write in an imperative language.

```haskell
swap :: STArray s Int a -> Int -> Int -> ST s ()
swap arr i j = do
    a <- readArray arr i
    b <- readArray arr j
    writeArray arr i b
    writeArray arr j a
```

###### Do one partition
`lift $ readArray arr i` makes an action with type `StateT Int (St s) a`, which can be shoved into the next monadic function. The same thing happens in `lift $ swap arr i pivotIdx`. See how `ST s` is used like `IO`, the `s` part in the type is actually a phantom type to constrain the underline scope of `STRef`.

```haskell
partition' ::
    (Ord a) => STArray s Int a -> a -> Int -> StateT Int (ST s) ()
partition' arr pivot i = do
    pivotIdx <- get
    this <- lift $ readArray arr i
    when (this < pivot) $ do
        lift $ swap arr i pivotIdx
        put (pivotIdx + 1)
```

###### Partition
Set piovt like.

Note `execStateT :: (Monad m) => StateT s m a -> s -> m s` execute the state and get the `s` back. `pivotIdx0` is the inital state we provide.

Note `mapM :: (Monad m) => (a -> m b) -> t a -> m t b` collect the result automatically, it works like `sequence . fmap` act on monadic function.

```haskell
partition :: (Ord a) => STArray s Int a -> Int -> Int -> ST s Int
partition arr start end = do
    pivot <- readArray arr start
    let pivotIdx0 = start + 1
    finalPivotIndx <-
        execStateT
            (MapM (partition' arr pivot) [(start + 1) .. (end - 1)])
            pivotIdx0
    swap arr start (finalPivotIndx - 1)
    return $ finalPivotIndx - 1
```

###### A helper
Perform the recursive partition. Wrap this logic in another function allows us to encapsulate the mutation, and provides a pure interface.
```haskell
helper :: (Ord a) => Int -> Int -> STArray s Int a -> St s ()
helper start end stArray = when (start + 1 < end) $ do
    pivotIdx <- partition stArray start end
    helper start pivotIdx stArray
    helper (pivotIdx + 1) end stArray
```

###### Assemble everything
`runSTArray` perform the action of a `ST` monad, and return  an immutable `Array`.

Note `thaw` here still copy the array, to do a implace thaw you can use `unsafeThaw`

Note `bounds` give the range of the array.

```haskell
qsort :: (Ord a) => Array Int a -> Array Int a
qsort arr = runSTArray $ do
    stArray <- thaw arr
    let (min, max) = bounds arr
    helper min (max + 1) stArray
    return stArray
```

#### ST
The inplace implementation of quick sort use mutable references based on `ST s` monad.

#### Some utilities from Data.STRef
These functions look really similiar to those `State` provides. The difference is `State` is functional state, but `ST` is real mutation.

These are all functions exposed by `STRef`, all other mutable opeartions will build on top of them.

##### `STRef s a`
A reference to mutable variable in the state thread `s`, containing a value of type `a`

##### `newSTRef :: a -> ST s (STRef s a)`
Create a new `STRef` in the current state thread

##### `readSTRef :: STRef s a -> ST s a`
Read the value of the `STRef`

##### `writeSTRef :: STRef s a -> a -> ST s ()`
Write into the `STRef`

##### `modifySTRef :: STRef s a -> (a -> a) -> ST s ()`
Mutate the content of `STRef`. The function is not applied strictly, so if the value get mutated but is seldomly used, the thunk will accumulate and cause memory leak. `modifySTRef'` provides strict version of the function.

Note `STRef s a` has `Eq` instance to compare the pointer identity.

```haskell
runST $ do
    ref <- newSTRef "Hello"
    writeSTRef ref (x ++ " world")
    readSTRef ref
```

#### ST monad
`data ST s a`

##### `runST :: (forall s. ST s a) -> a`
`runST` uses rank 2 type. value of `s` in `ST` is never accessible, it is actually a phantom type.

##### `fixST :: (a -> ST s a) -> ST s a`

#### Converting ST to IO
`stToIO :: ST :: ST RealWorld a -> IO a`
See `ST` and `IO` are pretty much the same. Here the phantom type `s` of `ST` is replaced with `RealWorld`, which gives you an IO...

There is also an similiar `ioToST :: IO a -> ST RealWorld a`.

`data RealWorld` is a primitive type... It is only used in the type system, as programmer you never touch it.

## Zipper
Zipper is an idiom for more efficient immutable data modification. Traditinally if you want to modify a node of a immutable binary tree, you need to traverse to the node, and construct a new tree with a new ndoe. If the node is deeply nested, it will be very inefficient. Zipper solve this problem by simply observed the fact that change the root of the node can be very efficient since it is not nested at all, nothing in the path need to be rebuild. So when using a zipper, each time address a value in the tree, it will change the focus to that node, and make the node as the root of the tree. By doing so modification will only involve change of the root node.

For list zipper we can represent like this
```
(2 1) 3 (4 5 6)
```
Where `3` is the current focus. Note the order of the left part is reversed, it's because if it's implemented as a list, `head` is far more efficient than `tail`; and because logically `2` and `3` are logically connected, it should be easier to reach `2`. Reverse the order helps us to do that.

An example is list zipper, which can be defined like this

```haskell
--          goForward   3
--  2   4  --------->   2
--  1 3 5               1 4 5
data ListZipper a = ListZipper {
    left :: [a]
    focus :: !a
    right :: [a]
} deriving (Show, Read, Eq)
```

##### conclusion
A way to look at zipper is it is a updateable pure funtional cursor into a data structure. It helps to keep track of element deelpy
nested in the data structure,

## Benefits Immutable data
- Easier to reason about
- Can be share across thread without worry about data racing.
- We can have the old version of the data (modify by copying).
