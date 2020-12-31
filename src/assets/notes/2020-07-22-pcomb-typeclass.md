-- tag note haskell parser typeclass
-- title Note: some parser combinator stuffs.
-- date 2020-07-22
-- source https://stackoverflow.com/questions/29861388/when-is-it-useful-to-define-multiple-lifetimes-in-a-struct
          https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html
          https://www.seas.upenn.edu/~cis194/fall16/lectures/09-more-applicative.html
;;
# Parser combinator in Haskell (in repo Notes/haskell..pinciple../parser )
##### 1. Parser
``` haskell
newtype Parser a = Parser {parse :: String -> [(a, String)]}
```
Essentially a function

##### 2. It looks very similar with State.
Parser is probably one of the most typical use case for a state monad.  You need some place to hold the position in current input stream for the entire parsing process. You can probably implement a parser with StateT alone.

##### 3. What is State after all?
Although it is called `State`,
```haskell
newtype State s a = {runState :: s -> (a, s)}
```
It's really just a reader that transform current state to the next one, and the actual state is the parameter of runState. So essentially State encapsulate all the computation involves with the state `s` in to a monad where the state is passed as parameter by default.

##### What is context?
It's a very sloppy note with bunch of heuristics. I already modified this text once but still doesn't seem quite right. Maybe the only way to know it better is to learn some type theory.
When doing `Functors` or `Monad` people talks about context without good examples. First to denote a context with type system you need higher kinded type. With a type `f a` of kind `* -> *` you can have `f` as a type operator with a type parameter `a`, or a type indexed by `a`. And when you are defining things like functor you define `fmap :: (a -> b) -> f a -> f b`, which in type level you are saying there exists a `fmap` implementation for this higher kinded type that if you pass `a -> b` and a value of this type it will gives you `f b` back. These are all denoted at type level, so no real context involved so far.
But when you are defining the `fmap` in term level, you can bring some meaning of the context. For instance, if you have `Maybe a`, the value of type `Maybe a` can be `Just a` or `Nothing.` It's the present of `Maybe` in type system make it possible, or `Maybe` attached more meaning to `a`.
Another example `State s a`. At type level you can think it as value with type `a` surrounded by `State s`. In definition `runState :: s -> (a, s)` is what really defines the `context`. It's a function reads a value s with type `s`, and return the value `a` and a new state. While executing `runState` the context will always exists along a, which is the point of state (pass the state implicitly).

##### Parser combinator
- Each parser is a function.
- You can make more complicated parsers based on simpler one by combining one parser function to another.
- In Haskell some of combinators can be achieve by implementing some typeclass. For instance, Alternative can be option combinator since they have the same type.

##### Common combinators
item: parse for any character.
sequence: parse one thing, then parse another thing. (can be thought as monad >>)
option: parse either one or another. (alternative <|>)
some: like regex +
many: like regex *
oneOf: parse if the string match one of the element
chianl: for parsing left recursive grammar
These are some basic combinators.

##### Parsec
Everybody use library to write parser.
- Define Tokens with Tok.LanguageDef () // a record of all token definitions.
- Define lexers `Tok.TokenParser ()`
- Define your AST
- Parse from token into AST
- Eval or codeGen depends on what you want to do
