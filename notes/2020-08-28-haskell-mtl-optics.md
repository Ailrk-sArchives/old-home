-- tag note haskell mtl
-- title Some mtl and lens
-- date 2020-08-28
-- source https://www.youtube.com/watch?v=GZPup5Iuaqw
          http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
;;
# Haskell. mtl, some lens, some concurrency

## MTL
A library helps to use monad transformer easier.

#### Review some monad transformer

You want monad transformer to stack a monad on top of another, and lift the outer monad action into the transformer with function `lift :: (Monad m, MonadTrans t) => m a -> t m a`.

Why this is useful? Imagine you have a maybe monad transformer `MaybeT IO a`, essentially what you are saying is you have a value `IO (Maybe a)`, and depends on how you define the monad instance it's bind probably be very similar to the primitive `Maybe`. But what if now you want to perform an `IO` action on this monad? You can do `lift putStrLn` which lift `putStrLn :: IO ()` into `MaybeT IO ()`.

This is a very generic concept and can be used to stack arbitrary monads. For instance, you have state monad transformer `newtype StateT s m a = StateT { runStateT :: \s -> m (a, s)}`; writerT `newtype WriterT w m a = WriterT { runWriterT :: \w -> m (a, w)}`; readerT `newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a}` and so on. The way to use them is similar. To use the original state or reader you just use the monad instance for the transformer. To use the wrapper monad `m`, you lift the action into the transformer.

You can tell the monad transformer is also a monad, so you can stick another layer of monad transformer on top of that, and make super complicated monad transformer.

Problem occurs when you have too many monads stacked together. To lift a specific monad action you might need multiple lift, like `lift . lift . lift action`, or if you want to return some stuufs you need `return .return . return`. This problem is already very obvious when I was doing the maybe monad exercise, and it could only be worse when the monad transformer get bigger and bigger.

#### Some monad transformer laws

Some invariants you can expect when using monad transformer.

```haskell
-- Law1
lift . return = return

-- Law2
lift (m >>= f) = lift m >>= (lift . f)
```

Note a monad transformer tranform monad to monad. If we write down the kind we have

```haskell
-- A monad
Monad (m :: * -> *)

-- A monad transformer tranfer monad to monad
MonadTrans (t :: (* -> *) -> * -> *)
```

Note A monad transformer composes outside in but unrolls inside out.

#### Some example with monad transformer

Make a composite computation with both `Reader` and `Maybe` monad.

```haskell
{-# LANGUAGE LambdaCase #-}
import Control.Monad.Reader

type Env = [(String, Int)]

-- your specific monad composition
-- Eval a is a Reader wrapped in maybe, so the result of runReader can be
-- Nothing.
-- Although it's a reader monad transformer, the Reader monad is actually
-- get wrapped inside the maybe monad. This is why monad transformer get
-- composed outside in.
type Eval a = ReaderT Env Maybe a

data Expr
   = Val Int
   | Add Expr Expr
   | Var String
   deriving (Show)

-- So now you are using a ReaderT, so you know you can just use it
-- as usual Reader monad. But besides that, now you can call
-- Maybe actions with a lift.
eval :: Expr -> Eval Int
eval = \case
  Val n -> return n
  Add x y -> do
    a <- eval x   -- normal monad bind.
    b <- eval y
    return $ a + b
  Var x -> do
    env <- ask -- use it as a reader, get the environment.
    val <- lift (lookup x env) -- use maybe monad.
    return val
-- the fact you need to lift the Maybe action is why you unroll monad
-- inside out.

env :: Env
env = [("x", 2), ("y", 5)]

ex :: Eval Int
ex = eval (Add (Val 2) (Add (Val 1) (Val "x")))

example1, example2 :: Maybe Int
example1 = runReaderT ex env
example2 = runReaderT ex []
```

#### How Reader, Writer, State monad are actually implemented

There is no implementation like this one in libraries.
```haskell
newtype State s a = State { runState :: s -> (a, s) }
instance Monad (State s) where
  return a = State $ \s -> (s, a)
  (State x) >>= f = State $ \s ->
    let (a', s') = x s
     in runState (f a') s'

newtype Reader r a = Reader [ runReader :: r -> a ]
instance Monad (Reader r) where
  return a = Reader $ \r -> a
  (Reader x) >>= f = Reader $ \r -> runReader (f $ x r) r
```

They actually are defined as more specific version of there corresponding monad
```haskell
type Reader r a = ReaderT r Identity a
type State s a = StateT s Identity a
type Wiriter w a = WriterT w Identity a
```

Again, what is identity monad? `Identity` is to mondas, functors, applicative as 0 to numbers. You use it when you want a monad to do nothing.

```haskell
newtype Identity a = Identity {runIdentity :: a}
instance Functor Identity where
  f <$> m = Identity (f $ runIdentity m)
instance Applicative Identity where
  fa <*> fm = Identity $ (runIdentity fa) (runIdentity fm)
instance Monad Identity where
  return a = Identity a
  m >>= f = k (runIdentity m)
```
It's one of those monad that stand lone they seems pretty useless, but are necessary when they get composed with other monads.

A benefit of it is, say you have a reader monad, it will have the same function to call as the reader transformer because it's defined for the more generic form. `ask` is defined for reader transformer so you can call it on any reader transformers.

#### mtl library
We mentioned above that too many layers of monad transformer is awkard to use, you need to have right amount of `lift` to reach the level you want, and it doesn't seem to scale very well: it's hard to imagine to manipulate a 20 layers tall monad transformer.

This is the motivatoin of `mtl` library. `mtl` is the most popular way to manage effect with monad in hasekll. You can avoid use monad transformer at all with algebraic effect and `fuesd-effect`, but if you want to use a lot of monads `mtl` is a mature solution.

#### How to use mtl
When you are using mtl you dont't really use *T style transformer all the time. Some example instances from mtl

```haskell
instance Monad m => MonadState s (StateT s m)
instance Monad m => MonadReader r (ReaderT r m)
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m)
```

This solve the multiple lift problems a of multiple transformers. mtl is by far the most popular effect system haskell ppl use.


#### New type deriving
Common technique being used with `mtl`. We already know new type is a very good way to let compiler help us differentiate two same types with different semantic meanings. There are other ways to use new types.

With newtype deriving and `mtl` we can produce flattened transformer types without needing to do endless lift and return.

An example of a stack machine.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

type Stack = [Int]
type Output = [Int]
type Program = [Instr]

-- What it is saying is that VM is a combination of
-- Readable program, writable output, and a stack state.
type VM a
    = ReaderT Program
    ( WriterT Output
    ( State Stack)) a

-- Comp is just a newtype wrapper on top of VM. You can derive
-- typeclass for it.
-- See how many code you can derive
newtype Comp a = Comp { unComp :: VM a }
  deriving (Functor, Applicative, Monad,
            MonadReader Program,
            MonadWriter Output,
            MonadState Stack)
data Instr = Push Int | Pop | Puts

-- With mtl derived, you don't need lift all the time to
-- use monad on different layers.
evalInstr :: Instr -> Comp ()
evalInstr instr = \case
  Pop -> modify tail  -- using the `State Stack`
  Push -> modify (n:)
  Puts -> do
    tos <_ gets head
    tell [tos]        -- using the `WriterT Output`

-- Just another example of using the transformer.
eval :: Comp ()
eval = do
  instr <- ask  -- using ReaderT
  case instr of
    [] -> return ()
    (i:is) -> evalInstr i >> local (const is) eval -- using ReaderT again

-- What it does:
-- * flip evalState parameter, so we can setup the initial stack [].
-- * note eval :: Comp (). It is our comp in term level.
--   unComp get the underlying value out from the new type.
-- * note runReaderT :: (r -> m a) -> ReaderT r m a
-- * note execWriterT :: WriterT w m a -> m w
-- You unwrap the monad transformer with the same order of how transformers
-- are stacked together.
execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)

-- Define the program
program :: Program
program = [
  Push 42,
  Push 27,
  Pop,
  Puts,
  Pop ]

-- * note mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
--   It's the function used to execute effect over a Fodable
main :: IO ()
main = mapM_ print $ execVM program
```

#### Monad morphisms with mmorph
You can `lift :: Moand m => m a -> t m a` to lift an action into the transformer, but also there are cases that you want to transform your existed monad transformers to new transformers; such transformation can be done with the help of `mmorph` library.

```haskell
-- map a function from a base monad to a function over a transformed monad.
hoist :: Monad m => (forall a. m a -> n a) -> t m b -> t n b

-- generalize an Identity monad into another monad.
-- For instance, we already know that State s a = StateT s Identity a.
-- With generalize we can turn StateT s Identity a to StateT s m a.
generalize :: Monad m => Identity a -> m a
```

Example of usage

```haskell
import Control.Monad.State
import Control.Monad.Morph

-- define a state as StateT [Int] Identity a
type Eval a = State [Int] a

runEval :: [Int] -> Eval a -> a
runEval = flip evalState

pop :: Eval Int
pop = gets head >>= \top -> modify tail >> return top

push :: Int -> Eval ()
push x = modify (x:)

ev1 :: Eval Int
ev1 = push 3 >> push 4 >> pop >> pop

-- hoist and generalize help you bring the State monad into
-- a state monad transformer with IO stacked on top.
ev2 :: StateT [Int] IO ()
ev2 = hoist generalize ev1 >>= \result ->
  liftIO $ putStrLn $ "Result: " ++ show result
```

#### Draw back of mtl

There are lots of problems with `mtl`.

###### n² instance problem / instance boilerplate problem.
To add a new custom transformer into a existing `mtl` transformer stack we need to derive a large amount of instances that do nothing but mostly lift.

###### Monad transformer composition is not commutative.
```haskell
stateExcept :: StateT s (Except e) a -> s -> Either e (a, s)
stateExcept m s = runExcept (runStateT m s)

exceptState :: ExceptT e (State s) a -> s -> (Either e a, s)
exceptState m s = runState (runExceptT m) s
```
