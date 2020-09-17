-- tag note haskell parallelism concurrency dip lazy
-- title Haskell data parallelism, concurrency, and polymorphic size array
-- date 2020-09-11
          https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial
          https://myslide.cn/slides/12046
          https://stackoverflow.com/questions/59586452/haskell-parallel-computation-and-the-sequential-property-of-monads
          https://slikts.github.io/concurrency-glossary/
          https://www.info.ucl.ac.be/~pvr/VanRoyChapter.pdf
          https://wiki.c2.com/?FunctionalReactiveProgramming
          https://www.stackbuilders.com/tutorials/haskell/image-processing/
;;
# Data Parallelism, Concurrency and polymorphic size array in haskell

## Buzzwords
Terminalogy used in this topic is very chaotic, it's worthy to organize them a bit.

#### Concurrent vs sequential
Concurrent: indepedent computations that can be executed in any arbitrary order.

Sequential: Computation follows a specify order.

#### Parallel vs serial
Parallel: executing multiple computations at the same time.

Serial: Ont at a time.

###### CPU bound vs IO bound

CPU bound: most of the time is spent on computation.

IO bound: most of the time is spent on waiting for IO.

###### Overlapping vs interleaved lifetimes

Overlapping lifetime: True parallel, or the execution is physically separated at the same time.

Interleaved lifetime: Conceptually parallel, but the actual exection can have specific order.

#### Scheduling
Assigning processor time to different logically executable computations.

#### synchronization
A scheduling that blocks executions to wait for a dependency. Problems that requires no synchroization are called `embarrassinly parallel`...

#### Preemptive vs cooperative
preemptive: Interrupting running computations to be resumed later.

cooperative: Each computation give out control voluntarily.

#### Control flow vs data flow (Imperative vs declaritive)
control flow: the path the point of execution takes in a program. Sequential programming focuses on explicit control flow structures like loop is called imperative programming.

data flow: Empahsis on the routing and transformation of data. Data flow is part of the declarative programming paradigm. Control flows data and computations are executed implicitly based on data availability.

#### Control dependencies vs data dependencies
dependency analysis: Determing ordering constraints between computations. The theory distinguishes between control and data dependencies.

control dependency: Order of execution.

data dependency: AKA true dependency

Data flow model focus on data dependencies and allow avoiding spurious dependencies like accidental locking. Dataflow model is inheently simplify modeling more concurrent or indepednent computations.

#### Nondeterministic vs deterministic
nondeterministic: The path of execution isn't fully determined by the specification of the computation, so the same input can produce different outcomes.

deterministic: deterministic computation is guaranteed to be the same.

#### Internal nondeterministic vs external nondeterministic
Computations can have internally nondeterministic paths of execution, but the nondeterminism can be abstracted away by the cmputational model and not observable externally.

You try not to have obvervable nondeterminism because is will cause inconsistent state. Race condition is an example of an observable nondeterminism.

Using lock primitives to contorl concurrency means leaving it up to programmer discipline to avoid accidental observable nondeterminism.

Declarative concurrency models impose a data dependency based order of execution, thus reduce the need to make trade off between concurrency and correctness.

#### Thread of exection and blocking thread

thread of exection: It's generally a synonymous with control flow.

blocking thread/threaded state: Core abstraction of imperative programming. In language like C, the blocking of thread offers the advantage of relatively close to the machine models.

#### System thread vs user thred / green thread / lightweight thread
System thread: heavyweight. It needs to allocate memory for stack, and context switching needs to switch between kernal mode and user mode.

Green thread: Don't need an os context switching so much faster to spawn than a system thread.

Green thread can still be parallelized by being scheduled onto different systme threads. For instance, in M:N model, M number of green threads are scheduled into N number of system threads. In this vain, nodejs is N:1 model, and apache is 1: 1 model.

#### Shared state vs message passing (distributed state)
In shared state: memory is shared by cooperating processes, which can exchange information by reading and writing data

In message passing: communication takes place by means of messages exchange between cooperating processes.

#### Asynchronous vs synchronous
asynchrnous: not happen at the same time. asynchrnous message passing is a communication model that does not require the sending and receiving to be synchronized.

A sequential model like the blocking thread can be extended to support concurrency by overloading synchronous calls with asynchronous semantics, meaning the call site does not create a dependency on the results of the asynchronous computation; instead, a runtime scheduler passes the results to a handler at a later time or asynchronously.

With first class functions, this can be modeled with cps.

#### System thread vs events
Events: messages used with a runtime scheduler called an event loop that dispatches messages fro a message queue to event handler.

Side note: lower level counterpart of event handlers are handware interrupt handlers.

#### Communicating sequential processes (CSP)
A concurrency model taht conbines the sequential thread abstraction with synchronous mesage passing communication. The message passing primitive is a blocking queue the blocks both on send and receive, meaning the concurrent computations perform a rendezvous.

#### Actor model
Similiar abstraction to CSP but using asynchrnous mesage passing communication (mailbox), where each actor can have a named identity, some mailboxes, and can send message to their own mailboxes.

## Parallelism and concurrency

All about making program faster by spliting a task into smaller chunks and execute each one separatly. If you want to parallelize some operation you might want raw system threads or separate processes occupy all yoru cpu cores. By gostav's law you also try to increase the ratio of parallelizable job in your code to improve performance gain.

Concurrency is the more generic version of parallelism. You also care about making things happen at the same time. Concurrency is about they way of structuring program but parallelism is more about the natural of the hardware that allows you to run a piece of code at the same time. Concurrency is frequently be used to address implementation of non blocked io

There're some common approach to achieve concurrency. Shared state and message passing are two most famous one. In haskell we have software transational memory, which treat each access to shared memory as an atomic operation like how database does. There is also deterministic concurrency model, they're less expressive than nondeterministic concurrency model, but at the same time they're much easier to write: determinism guarantee no race conditon.

#### Synchronization, deadlock and race condition.

The most primitive approach to control shared state in a concurrent program is to use synchronization primitives. Mutex, semaphores are some common examples.

The problem is that locks are hard to manage. It's hard to reason about if your lock design cover all possible coner cases. In a bad senario two locks ended up waiting for each other you will get a dead lock.

On the other hand, if your concurrent codes access the same state in a manner that one thread modify the state before another should read it. In that case yor state is no longer consistent and you have a race condition.

Note: like how lazyniess is called a lazy abstraction because you need to be aware of it's existence to avoid memory leak, synchronization primitive can also be a leaky abstraction in the sense that to achieve the end goal of getting a concurrent system you must be aware of the existence of lock and how they affecthe order of execution.

#### Concurrency in haskell

If we take a step back, and reason about what makes traditional concurrency hard to program, we might conclude that things change all the time and it's hard to keep track of. So naturally, what if we don't have mutable state at all? Nothing get changed, so there is no such a concept of keep track of stuffs; no matter what order threads are executed on, they don't modify state, thus no race condition either. If you use lock it means you want to protect some state from being modified while you are doing things. So... since no state, it means no lock, and no dead lock.

This is the ideal world, because in reality if all your objects are absolutely stale they there is nothing interesting of it. But it does show immutability can improve the situation al lot.

In haskell side effect is separated into `IO` monad or `ST s` monad, and everything else are pure functions. If your functions are pure, you don't need to care too much about state change either. Even state monad is only visible by it's own scope, you don't have a state monad that shares state with other functions.

###### Green threads
Haskell has support for lightweight green thread, a type of thread pool that managed by the language itself rather than the operating system. The process of creating a thread entails context switching, which is a very expensive operation. If you need to spawn and join massive amound of threads frequently, you will soon ended up to snow ball the effect and become less efficient as you would expect. (Of course this implies you frequently do context switching, if threads just sit there the overhead is nothing). A green thread, instead, has the language's runtime system emulates the system thread without the need of os context switching. No OS context switching means no switching back and forth between user mode to kernal mode, and the whole scheduling can be made much more effcient.

Green thread is essentially asynchronous io, you don't get too much benefits on doing data parallelism. But note the difference between green thread and node js's event loop approach: event loop can not utilize all cpus, but green threads can span all cores as with no problem.

#### Polymorphic size array
In most other languages it's called ndarray, but it's haskell so you'd expect some fancy words. Haskell has some pretty advance numpy equivalent. Comparig with numpy, theses libraies tends to have much stronger emphasis on type safty.

#### Automatic data parallel
Convert sequential code into multi-threaded or vectorized code automatically. Openmp can be one of the example.

#### Flat parallelism Nested data parallelism
Flat parallelism when your work only contains scalars instead of some nested structures. Flat parallelism is well understood and have a lot of implementations already.

Nested data parallelism roughly means while you are parallelizing an array, you find you need to parallelize some other arrayes; there are nested structures in the to-be parallelized data.

#### Repa
A haskell equivalent of numpy, the main salling point is all the implementation are entirely on haskell, you don't need dependencies like blas and lapack. Also it handles automatic data parallelism.

Array has type `data family Array rep sh e`, which rep can be either unboxed `U` or boxed `A`. sh is the shape of the array, denoted as (Z :. *).

Some examples. It gives a taste of how to do data parallelism in haskell.

```haskell
import System.Environment
import Data.Word
import Foreign.Ptr
import qualified Foreign.ForeignPtr.Safe as FPS
import qualified Data.Vector.Storable as V
import qualified Data.Array.Repa as R
import qualified Data.Repa.Repr.ForeignPtr as RFP
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL

i, j, k :: Int
(i, j, k) = (255, 255, 4)

-- cycle :: [a] -> [a]
v :: V.Vector Word8
v = V.fromlist . take (i * j * k) . cycle $ concat
    [ [ r, g, b, 255]
      | r <- [0 .. 255]
      , g <- [0 .. 255]
      , b <- [0 .. 255]]

ptr2repa :: Ptr Word -> IO (R.Array RFP.F R.DIM3 Word8)
ptr2repa p = do
    fp <- FPS.newForeignPtr_ p
    return $ RFP.fromForeignPtr (Z :. i :. j :. k) fp

-- an example of matrix - matrix multiplication
-- note operations like transpose will not actually create a transpose
-- but rather change the way of indexing. So it's cheap to use.
mmMult :: Monad m
       => R.Array U R.DIM2 Double
       -> R.Array U R.DIM2 Double
       -> m (R.Array U R.DIM2 Double)
mmMult a b = sumP (R.zipWith (*) aRepl bRepl)
    where
        t = R.transpose2D b
        aRepl = extend (Z :. R.All :. colsB :. All) a
        bRepl = extend (Z :. rowsA :. All :. All) t
        (Z :. colsA :. rowsA) extent a
        (Z :. colsB :. rowsB) extent b

-- computeP :: Array -> m Array
-- it uses a strict monad to enforce sequential operation, thus avoid
-- nested data parallelism.
imageDesaturate :: R.All U R DIM2 Double -> IO ()
imageDesaturate = do
    [f] <- getArgs
    runIL $ do
        (RGB a) <- readImage f
        b <- (computeP $ traverse a id luminosity) :: IL (R.Array R.F R.DIM3 Word8)
        writeImage ("grey-" ++ f) (RGB b)
    where
        luminosity :: (R.DIM3 -> Word8) -> R.DIM3 -> Word8
        luminosity _ (Z :. _ :. _ :. 3) = 255   -- alpha
        luminosity _ (Z :. i :. j :. _) = ceiling $ 0.21 * r + 0.71 * g + 0.07 * b
        r = fromIntegral $ f (S :. i :. j :. 0)
        g = fromIntegral $ f (S :. i :. j :. 1)
        b = fromIntegral $ f (S :. i :. j :. 2)

main = do
    r <- V.unsafeWith v ptr2repa
    runIL $ writeImage "test.png" (RGBA r)
    imageDesaturate
    return ()
```

#### Sequential and monad
```
It's not because monad somehow represents sequential operations, is that anything worth naming "sequential" with it are inherently monadic.
```
