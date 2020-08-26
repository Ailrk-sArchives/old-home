-- tag note haskell
-- title Haskell Idioms
-- date 2020-08-17
-- source https://kowainik.github.io/posts/haskell-mini-patterns
;;
# Haskell idioms
Some commonly seen haskell idioms. I found this blog post very insteresting. There was a similar article [for C](https://github.com/mcinglis/c-style).

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

## Make illegal states unrepresentable

## Phantom type parameters

##  MonadFail Sugar

## Polymorphization

## Bidirectional parsing

## Recursive go
