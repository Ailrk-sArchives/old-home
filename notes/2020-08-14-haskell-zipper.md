-- tag note haskell data-structures zipper ST
-- title Some more haskell notes. ST zipper
-- date 2020-08-14
-- source https://en.wikibooks.org/wiki/Haskell/Zippers
          https://github.com/xmonad/xmonad
          https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell
          https://wiki.haskell.org/Monad/ST
;;
# ST, Zipper, some other typeclasses.

## ST monad
`STRef` is a more generic form of `IORef` that allows you to have mutable reference. `ST s a` is a `state` like monadic interface helps you to use `STRef`. `runST :: (forall s. ST s a) -> a` is the function to execute a `ST` computation, and it use `rank 2 type` to enforce the mutable reference is always to each `runST`, so that the side effect can only happen inside the `runST`, and externally behave like a pure function.

#### A very cool implementation of quick sort with in place mutation in haskell
Without mutation, to write a quick sort is actually easier. Like this:
```haskell
quickSort1 :: (Ord) => [a] -> [a]
quickSort1 [] = []
quickSort1 (x:xs) =
    let left = quickSort1 [a | a <- xs, a <= x]
        right = quickSort1 [a | a <-xs, a > x]
     in left ++ [x] ++ right
```

This version looks very neat, just doesn't perform very well. Because everything is immutable, so the runtime will create a new list in each iteration. Although the gc process is not too bad for immutable data, it is still not idea compare with a in place mutation.

With `STRef` and `ST s a` monad we can have inplace mutation while providing a pure interface. This allows us to implement algorithms that are more efficient with mutation.

##### Implementation
`STRef` and `STArray` are basical notion of dealing with mutation in haskell, and `ST s a` monad provides a monadic environment for the mutation to happen.

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

###### Partition loop
```haskell
partitionLoop ::
    (Ord a) => STArray s Int a -> a -> Int -> StateT Int (St s) ()
partitionLoop arr pivot i = do
    pivotIdx <- get
    this <- lift $ readArray arr i
    when (this < pivot) $ do
        lift $ swap arr i pivotIdx
        put (pivotIdx + 1)
```

###### Partition
```haskell
partition :: (Ord a) => STArray s Int a -> Int -> Int -> ST s Int
partition arr start end = do
    pivot <- readArray arr start
    let pivotIdx0 = start + 1
    finalPivotIndx <-
        execStateT
            (MapM (partitionLoop arr pivot) [(start + 1) .. (end - 1)])
            pivotIdx0
    swap arr start (finalPivotIndx - 1)
    return $ finalPivotIndx - 1
```

###### A helper
```haskell
helper :: (Ord a) => Int -> Int -> STArray s Int a -> St s ()
helper start end stArray = when (start + 1 < end) $ do
    pivotIdx <- partition stArray start end
    helper start pivotIdx stArray
    helper (pivotIdx + 1) end stArray
```

###### Assemble everything
```haskell
qsort :: (Ord a) => Array Int a -> Array Int a
qsort arr = runSTArray $ do
    stArray <- thaw arr
    let (min, max) = bounds arr
    helper min (max + 1) stArray
    return stArray
```
