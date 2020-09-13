-- tag note haskell parallelism concurrency STM
-- title Haskell data parallelism, concurrency, and polymorphic size array
-- date 2020-09-11
          https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial
          https://myslide.cn/slides/12046
          https://stackoverflow.com/questions/59586452/haskell-parallel-computation-and-the-sequential-property-of-monads
          https://slikts.github.io/concurrency-glossary/
          https://www.info.ucl.ac.be/~pvr/VanRoyChapter.pdf
          https://wiki.c2.com/?FunctionalReactiveProgramming
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

#### Internal vs external

#### Control flow vs data flow (Imperative vs declaritive)

#### Control dependencies vs data dependencies

#### Nondeterministic vs deterministic

#### Internal nondeterministic vs external nondeterministic

#### Thread of exection and blocking thread

#### System thread vs user thred / green thread / lightweight thread

#### Shared state vs message passing (distributed state)

#### Asynchronous vs synchronous

#### System thread vs events

#### Communicating sequential processes (CSP)

#### Actor model

#### Functional reactive programming in small

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

