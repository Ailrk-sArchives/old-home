-- tag note haskell parallelism concurrency STM
-- title Haskell data parallelism, concurrency, and polymorphic size array
-- date 2020-09-11
          https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial
          https://myslide.cn/slides/12046
          https://stackoverflow.com/questions/59586452/haskell-parallel-computation-and-the-sequential-property-of-monads
;;
# Data Parallelism, Concurrency and polymorphic size array in haskell
All about making program faster by spliting a task into smaller chunks and execute each one separatly. If you want to parallelize some operation you might want raw system threads or separate processes occupy all yoru cpu cores. By gostav's law you also try to increase the ratio of parallelizable job in your code to improve performance gain.

Concurrency is the more generic version of parallelism which you also care about making things happen at the same time. But when people talking about concurrency they focus more on topics of avoiding get blocked by IO bound operations. Green threads and async with epolls are all good examples.

Really, concurrency is a way of structuring programs. If you want to have async  io your entire io process will need to be asynchronous, if you want to use actor model your program logics are also get organized by actors run concurrenctly.

Concurrency are inherently non-deterministic, because can not guarantee the order of execution at any given moment. This problem is especially bad if you have shared state.

I know some people think concurrency and parallelism are othogonal concepts since one is more about performance and the other is more on program structures. But in some sense a parallelism program need to be a concurrent one, and the pain points are shared between these two concepts, so I still think concurrency is a generalization of parallelism.

#### Synchronization, deadlock and race condition.

The most primitive approach to control shared state in a concurrent program is to use synchronization primitives. Mutex, semaphores are some common examples.

The problem is that locks are hard to manage. It's hard to reason about if your lock design cover all possible coner cases. In a bad senario two locks ended up waiting for each other you will get a dead lock.

On the other hand, if your concurrent codes access the same state in a manner that one thread modify the state before another should read it. In that case yor state is no longer consistent and you have a race condition.

#### Concurrency in haskell

If we take a step back, and reason about what makes traditional concurrency hard to program, we might conclude that things change all the time and it's hard to keep track of. So naturally, what if we don't have mutable state at all? Nothing get changed, so there is no such a concept of keep track of stuffs; no matter what order threads are executed on, they don't modify state, thus no race condition either. If you use lock it means you want to protect some state from being modified while you are doing things. So... since no state, it means no lock, and no dead lock.

This is the ideal world, because in reality if all your objects are absolutely stale they there is nothing interesting of it. But it does show immutability can improve the situation al lot.

In haskell side effect is separated into `IO` monad or `ST s` monad, and everything else are pure functions. If your functions are pure, you don't need to care too much about state change either. Even state monad is only visible by it's own scope, you don't have a state monad that shares state with other functions.

###### Green threads
Haskell has support for lightweight green thread, a type of thread pool that managed by the language itself rather than the operating system. The process of creating a thread entails context switching, which is a very expensive operation. If you need to spawn and join massive amound of threads frequently, you will soon ended up to snow ball the effect and become less efficient as you would expect. (Of course this implies you frequently do context switching, if threads just sit there the overhead is nothing). However, a green thread

#### Polymorphic size array
In most other languages it's called ndarray, but it's haskell so you'd expect some fancy words. Haskell has some pretty advance numpy equivalent. Comparig with numpy, theses libraies tends to have much stronger emphasis on type safty.

#### Auto data parallel

#### Nested data parallelism and GPU

#### Repa

#### Sequential and monad
```
It's not because monad somehow represents sequential operations, is that anything worth naming "sequential" with it are inherently monadic.
```

I saw this some where and felt it is quite a powerful statement.

The famous meme `monad is a monoid in the category of endofunctors` is actual helpful to understand this statement here.  I don't know category theory, but I do know monoid. The main property of monoid is associativity.

#### Accelerate as an embeded langauge

