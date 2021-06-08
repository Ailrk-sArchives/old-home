-- tag haskell yet-another-one
-- title Yet another one of those #1: Monad
-- time 2021-04-01
-- source https://wiki.haskell.org/Monad_laws
          https://github.com/ailrk/misc/blob/master/haskell/src/Cat/Monads.hs
          https://wiki.haskell.org/All_About_Monads#The_monad_laws
;;
## Yet another one of those #1: Monad

Yes it's yet another haskell monad tutorial. Because the topic is so cliche, I won't even bother write an intro. If I write one it will be the same as others' anyway (purely functional, side effects whatever, balabala). I try to share some of my understanding of the semantics of monad and it's application in Haskell, which I found I struggle a bit when I was learning it.

I do feel the need to point out the purpose of the series though. As the name suggests, it's a series on topics that you can find articles everywhere. However, I find lots of articles are baised. When introducing something, they tend to amplify the advantages and ignore flaws. I want the series to be as faithful as I can, at least balance on pros and cons, and makes some comparisons. Of course I only know so much, so if you find the article funny and urge to teach me a thing or two, please contact me, more than appriciated!

## Ok, Monad

If you are reading this article hidden at the corner of the Internet, I'm sure you already learnt the definition of a monad in Haskell. Assume we all know:

1. Monad is a rigoroulsy defined concept in category theory.
2. Monad needs to be an Applicative first, which in turn needs to be a Functor.
3. In Haskell a monad supports `(>>=) :: m a -> (a -> m b) -> m b` and `return :: a -> m a`.

We extend from here.

## Missing context before Monad.

#### Technically everything is function in a lambda calculus based langauge.
When writing haskell, it's helpful to notice that everything is essentially function. Haskell is [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#:~:text=Lambda%20calculus%20also%20written%20as,to%20simulate%20any%20Turing%20machine.) based programming languages.

Lambda calculus is a computational model that initially intended to formalize function application, and ended up to be turing complete.

In pure lambda calculus, everything can be encoded by function abstraction and application. For instance,

In lambda calculus, there are either numbers, nor algebraic data type.



#### Neither Higher kinded type nor Typeclass is necessary for Monad.
Although monad is usually introduced with typeclass, it really has nothing to do with typeclass. Typeclass is a nice way to allow us to talk about something is a monad at the type level.


## Monad and CPS (Continuation passing)

## Monads that NOT in Haskell

## Monads in Haskell

## Nested structure

## Monad Law

## If Algo60 is in monad.

##


