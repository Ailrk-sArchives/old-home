-- tag haskell parser typeclass
-- title Note: Revist some parser combinator stuffs.
-- date 2020-07-22
-- source https://stackoverflow.com/questions/29861388/when-is-it-useful-to-define-multiple-lifetimes-in-a-struct
          https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html
;;
# Parser combinator in Haskell
1. How to define parser in haskell?
    ``` haskell
    newtype Parser a = Parser {parse :: String -> [(a, String)]}
    ```
    Essentially a funciton

2. It looks so similar with State.
    Parser is probably one of the most typical use case for a state monad.
    You need some place to hold the position in current input stream for the
    entire parsing process. You can probably implement a parser with StateT alone.

3. What is State after all?
    Althought it is called `State`,
    ```haskell
    newtype State s a = {runState :: s -> (a, s)}
    ```
    It's really just a reader that transform current state to the next one, and the
    actual state is the parameter of runState.

    So essentially State encapsulate all the computation involves with the state s
    in to a monad where the state is passed as parameter by default.

4. Some intuitions about typeclasses for Parser? like what does they do?
    Functor:
        simply apply a funciton to parsed result.
        You can use it to make a slightly different parser based on an existed one.
    Applicative:
        pure construct a Parser with the given result regardless what the state is.
        (<*>) just like how applicative work for List, but replacingf the state along the way.
    Monad:
        Again, like applicative, it works like List monad.
    MonadPlus:
        mplus means concat the result of two parser into one list.
        identity is a parser that return an empty list.
    Alternative
        an empty list is regard as the failure case.

5. Conclusion about typeclasses with context
    If a typeclass holds some context, it might has higher kind. For example, Monad is * -> *

    When you have the same typeclass for different types, something change, and something
    stay the same:

    The abstract notion of the typeclass will not change. Namely the typeclass will always
    works according to its type signature. For a functor the notion is to
    apply (a -> b) into the value in the functorial context. No matter what structure the
    context has, this goal will always be achieved.

    But what will happen in the context during the computation changes among type. For Parser
    Functor it will replace the old state with a new one, while for a Maybe functor
    will return Some or None value based on what value the function is applied to.

    But on the other hand, different typeclasses for the same type usually have similar effect.
    Functor, Applicative, Monad for State all substitute old state with the new one. Similarly
    those typeclasses for Maybe all return Nothing if a value is absent.

    It's a pretty rough intuition, I might need to learn some Cat theory?

6. Some intuition about parser combinator in general
    Each parser is a function. You can make more complicated parsers based on simpler one by
    combining one parser function to another. In haskell some of this combination mechanics can
    be regard as functionalities provided by typeclasses. For instance, Alternative is a very good
    choice for implement option combinator, you can just implement that.

7. How many parser combinators are there?
    item: parse for any character.
    sequence: parse one thing, then parse another thing. (can be thought as monad >>)
    option: parse either one or another. (alternative <|>)
    some: like regex +
    many: like regex *
    oneOf: parse if the string match one of the element
    chianl: for parsing left recursive grammar
    These are some basic combinators.

8. Parsing in a real project?
