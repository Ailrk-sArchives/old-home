-- tag note haskell boolean-blindess evidence idioms
-- title Haskell Idioms
-- date 2020-08-17
-- source https://kowainik.github.io/posts/haskell-mini-patterns
;;
# Haskell idioms
Some commonly seen haskell idioms. I found this blog post very insteresting. There was a similar article [for C](https://github.com/mcinglis/c-style). This note is just some practices and code snippets. I found this type of articles is very userful for people who already know core concepts of a language but want to be better at it.

Besides examples and exercises in the article, I also make some notes about part of haskell I didn't encounter before.

## Newtype
#### Description
Use `newtype` as lightweight data wrapper to represent semantically different data. For example, `String` vs `Password` vs `Username`. Wrapping data with newtype allows you to implement more typeclass wihtout affect the data get wrapped. Besides, you can also derive typeclass the wrapped data already defined.

Compiler will use `newtype` types for type checking only, and the wrapper will be erased in the final binary, so it's a haskell zero cost abstraction.

#### Example
Newtype accept only one parameter for it's data constructor.

```haskell
newtype PasswordHash = PasswordHash ByteString
newtype Password = Password {
    unPassword :: ByteString
}
validate :: Password -> PasswordHash -> Bool
validate = bool
```

Because `password` and `passwordHash` are `newtype`, compiler is aware of them as different types. On another hand, if you simply give them a alias by using `type`, error message will treate all of them as `ByteString`, not necessarily pretty.

## Smart constructor
#### Description
Haskell idiom for constructing values. Essentaily a function only used to creates a data type. You read ppl's code and see a function named `mk..` and return a Maybe or Either, those are smart construtors. Smart construtor helps to separate concerns and make illegal states unrepresentable.

#### Example
```haskell
newtype Password = Password ByteString

unPasword :: Password -> ByteString
unPasword (Password password) = password

-- smart constructor
mkPassword :: ByteString -> Maybe Password
mkPassword pwd
    | ByteString.null pwd = Nothing
    | otherwise = Just(Password pwd)
```
mkPassword gives another layer of validation and make sure the data it construct make sense. If all passwords are made from mkPassword then there will be no empty password in the program.

```haskell
import Control.Applicative (liftA2)
import Data.List.NoEmpty (NonEmpty (..))
newtype Tag = Tag String
newtype TagList = Taglist (NonEmpty Tag)

mkTag :: String -> Either String Tag
mkTag tag
    | null tag = error "Empty Tag"
    | otherwise = Tag tag

unTagList :: TagList -> NonEmpty Tag
unTagList (TagList taglist) = taglist

mkTagList :: [String] -> Either String TagList
mkTagLsts [] = Left "Empty"
mkTagList (tag:tags) =
  TagList <$> liftA2 (:|) (mkTag tag) (traverse) mkTag tags
```

#### PS
Note traverse is just fmap then sequence. `(:|)` is the constructor for `Data.List.NonEmpty`. `Data.List.NonEmpty` is a list that always has at least one element. (Except that it's exactly the same as a normal List).

#### PS
`(traverse) mkTag tags` fmap then sequence. If `t` is traversable and `f` is applicative, the result of traverse is `f (t a)`. In this case it produce `Either String [Tag]`

`liftA2` is a applicative lifting, it lifts `(:|)` into `(mkTag tag) :: Either String Tag` and makes partially applied function inside the applicative. Assume no errors happen, `Either String` shoule be `Right`, thus we have `Right ((mkTag Tag) (:|))`. `liftA2` will apply it with the `Either String [Tag]` formed by the traverse, so the final result is we cons the first tag to the rest of the tags.

## Evidence
#### Description
Replace boolean blindness with the validation witness. (But what is boolean blindness? What is validation witness?)

###### Bool Blindness
The situtation that you are using a if expression and boolean to test a data, and if it's ture do some operation on it. But after you passed the boolean check, you lose the checked information in the then scope. For example:

```haskell
and :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> Maybe Int
and f g x =
  if isNothing (f x) || isNothing (g x) then Nohting
    else Just $ fromJust (f x) + fromJust (g x)
```

You see the `fromJust` is copmpletely unnecessary since you already know `f x` is not Nothing.

###### Validation Witness
Basically telling you to use pattern matching. Or write your code in a way that the data in the branch is been narrowed to a specific case, so you are aware of the effort put on validation.

#### Example

```haskell
-- opaque data type that only been create with validateHash
data UserAuth
validateHash :: Password -> PasswordHash -> Maybe UserAuth

accessDenied :: User -> IO Page
getUserPage :: UserAuth -> User -> IO Page

loginUser :: User -> Password -> PasswordHash -> IO Page
loginUser user pwd pwdHash = case validateHash pwd pwdHash of
  Nothing -> accessDenied user
  Just auth -> getUserPage auth user
```

## Make illegal states unrepresentable
#### Description
The point of using smart constructor and evidence is to make illegal states unrepresentable. You want to write your code so a error state can't happen.

#### Example

```haskell
-- Bad
handleTwoOptionals :: Maybe a -> Maybe b -> IO ()
handleTwoOptionals (Just a) (Just b) = ...
handleTwoOptionals Nothing Nothing = No value...
handleTwoOptionals _ _ = error ...
```
This is a bad exmaple, there are two cases you don't need anyway. Why don't just make it a tuple.

Like this:
```haskell
-- Good
handleTwoOptionals :: Maybe (a, b) -> IO ()
handleTwoOptionals (Just (a, b)) = ...
handleTwoOptionals Nothing = No value...
```
You are expressing the same thing with less input state possible.

So a rule of thumb is to limit the amount of state possible, just like how you always try to make scope smaller.


## Phantom type parameters
#### Description
Phantom type doesn't have a value in the term level. You can phantom type parameter to indicate different usage of a newtype.

#### Example
```haskell
-- specify what id it is
newtype Id a = Id { unId :: Int }
isCommentByUser :: Id User -> Id Comment -> Bool

newtype Password a = Password { unPassword :: ByteString }
newtype PasswordHash a =
    PasswordHash { unPasswordHash :: ByteString }

-- You can express the passwordHash is for the same type of
-- password
mkPassword :: Password a ->  Maybe (PasswordHash a)
```

Another example
```haskell
import Data.Binary (Binary)
import Data.ByteString

newtype PrivateKey = PrivateKey ByteString
newtype PublicKey = PublicKey ByteString
newtype Signature a = Signature ByteString

createPublicKey :: PrivateKey -> PublicKey
createPublicKey = error "not implemented"

sign ::Binary a => PrivateKey -> a -> Signature a
sign = error "not implemented"

verifySignature ::
    Binary a => PublicKey -> Signature a -> a ->  Bool
verifySignature = error "Not implemented"
```
This ensures that you cannot verify signature for a different type that the signature was created initally.


##  MonadFail Sugar
#### Description
A way to handle nestesd pattern matching (or multiple parts of a data) when the reason of a particular failure is not important. Using MonadFail sugar you can get cleaner syntax for nested pattern matching while avoid creating partial functions. The least thing you want to have is make a partial function. It doesn't comply with the "make illegal state unrepresentable" rule, and it makes some potential runtime error implicit.

`MonadFail` is a typeclass with only one function that helps to skip the failure case in a monad without needing you to manually write out the pattern matching.

```haskell
class Monad m => MonadFail m where
    fail :: String -> m a
MonadFail fail the
```

`MonadFail` is implemented for some commonly used datatypes.  For instance, `IO` and `Maybe`.

See the example how `do notation` in a `IO` monad get desugared. In the example, if the pattern matching in the fist line of the do notation failed, it will fall back to a fail branch implicitly. A failed pattern mathing in `IO` will cause a IO exception been thrown, and it is handled by the implementation of `MonadFail`.

```haskell
-- sugared code
main :: IO ()
main = do
    [_, arg2] <- getArgs
    print args2

-- desugared
main :: IO ()
main = getArgs >>= \args -> case args of
    [_, arg2] -> print arg2
    _ -> fail "Some compiler generated message"
```

#### Example

```haskell
bufferLastLine :: Buffer -> Maybe String

cmdSequence :: Buffer -> Maybe String
cmdSequence buffer = do
    line <- bufferLastLine buffer
    ["CMD", number, cmd] <- Just $ words line
    42 <- readMaybe number
    pure cmd
```
There is a change that  `Just $ words line` fail to pattern match with `["CMD", number, cmd]`, and that's ok, because `Maybe` implemented `MonadFail`, the do notation actually desugared into

```haskell
cmdSequence buffer = bufferLastLine buffer >>=
    \case
        ["CMD", number, cmd] ->
            readMaybe number >>= \case
                42 -> pure cmd
                _ -> fail ""
        _ -> fail ""
```

Some other examples. `readMaybe :: (Read a) => String -> Maybe a`

```haskell
import Text.Read (readMaybe)

sumThree :: String -> Maybe Int
sumThree s = do
    [a, b, c] <- traverse readMaybe $ words s
    pure $ a + b + c

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs =  [n | Just n <- (f <$> xs)]

-- relies on that MonadFail sugar will handle our failure case.
threeNothing :: Maybe a -> Maybe b -> Maybe c -> Maybe ()
threeNothing a b c = do
    Nothing <- Just a
    Nothing <- Just b
    Nothing <- Just c
    pure ()
```

Note, `MonadFail` is a rough way to handle error cases. It's good for `Maybe` and `IO` since you might not care about the reason why they fail. But if you have Monads report the exactly reason of a failure, like `Either e a` or `Validation`, manually handle failure cases is required.

#### PS MonadFail and list comprehension

`MonadFail` for list comprehension helps pick values from a list. If the pattern matching is failed the value will simply not be included into the resulting list.
```haskell
sameRight :: [(Either e1 Int, Either e2 Int)] -> [Int]
sameRight xs = [n | (Right n, Right n) <- xs, n == m]
```

## Polymorphization
#### Description
Make function parameters as generic as possible. Make a function more generic means less code for each ad hoc cases, and better reusability of the same function under different senarios. You see most typeclass are fully polymorhic, and that make them applicable for almost everything.

#### Example
This example shows how to make a already very generic function even more generic.
```haskell
-- ok
partition :: (a -> Bool) -> [a] -> ([a], [a])
-- better
partition :: (a -> Either b c) -> [a] -> ([b], [c])
```
See how b and c can be the same type as a, but write the code this way allows us to differentiate which part of the tuple is the partion we filtered out, so more information can be stored in the type.

#### PS
The end goal is to encode as much information about the purpose of the code as possible into the type level while making it ass generic as possible. if a function only take concrete types, it is clear that there is a chance to make it polymorphic. But even for functions that are polymorphic, we can make it even more generic by introduce more type parameters for different roles.

Note, you might have different type parameters, but two different type parameters can totally refer to the same type. So that's why it's so common to introduce more type parameters, which covers the original case while enable new possibilities, in another word, more generic.

## Bidirectional parsing
#### Description
For any bidirectional converstion by implement one direction and get the other one by free. Works whenever you have injective function function.

#### Example
Say you have enum, you can make a function to convert the enum value to a string. But besides that you also want to do the opposite, that is given a string and return it's corresponding enum value. We can do that without write another conversion function that covers all cases. Instead we can make a helper function to help us to make the inversion.

```haskell
-- inverseMap from `relude package`
inverseMap :: forall e s. (Bounded e, Enum e, Ord s)
    => (e -> s) -> s -> Maybe e
data Color = Green | Yellow | Blue
showColor :: Color -> Text
showColor = \case
    Green -> "Green"
    Yellow -> "Yellow"
    Blue -> "Blue"
-- bidirectional parsing with one function
parseColor :: Text -> Maybe Color
parseColor = inverseMap showColor
```
A complete example.
```haskell
-- provides a finder grained control over how instance maybe derived.
{-# LANGUAGE DerivingStrategies #-}

-- match on multiple patterns on parameter of lambda
{-# LANGUAGE LambdaCase #-}

-- Overload string literal to any `isString`
{-# LANGUAGE OverloadedStrings #-}

-- Basically spreading operator.
{-# LANGUAGE RecordWildCards #-}
Module Bi where
import Data.Text (Text)
import Relude.Extra.Enum (inverseMap)
data Color = Red | Green | Yellow
    deriving stock (Show, Enum, Bounded)
showColor :: Color -> Text
showColor = \case
    Red -> "red"
    Green -> "green"
    Yellow -> "yellow"
    Blue -> "blue"

parseColor :: Text -> Maybe Color
parseColor = inverseMap showColor

data FruitName = Apple | Orange | Lemon | Blueberry
    deriving stock (Show, Enum, Bounded)

showFruitName :: FruitName -> Text
showFruitName = \case
    Apple -> "apple"
    Orange -> "orange"
    Lemon -> "lemon"
    Blueberry -> "blueberry"

parseFruitName :: Text -> Maybe FruitName
parseFruitName = inverseMap showFruitName

-- strictly evaluated
data Fruit = Fruit {
    fruitColor :: !Color
    fruitName :: !FruitName
} deriving stock (Show)

parseFruit :: Text -> Maybe Fruit
parseFruit t = do
    -- pattern matching which fail case handled by MonadFail
    [color, name] <- Just $ Text.words t
    fruitColor <- parseColor color
    fruitName <- parseFruitName name
    pure Fruit{..}  -- From RecordWildCards extensions
                    -- fit fruitName and fruitColor directly.
```

## Recursive go
#### Description
Moving recursion over data types into another function. By doing so you can have the recursion function pass some parameters down as extra information without expose this detail to the caller of the interface.

#### Example
```haskell
-- accumulator i is not exposed to the external world.
atGo :: [a] -> Int -> Maybe a
l `atGo` n = go 0 l
    where
        go :: Int -> [a] -> Maybe a
        go _ [] = Nothing
        go i (x:xs) = if i == n then Just x else go (i + 1) xs
```

