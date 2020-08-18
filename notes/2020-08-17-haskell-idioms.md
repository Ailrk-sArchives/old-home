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
Haskell idiom for constructing values.

#### Example


## Evidence

## Make illegal states unrepresentable

## Phantom type parameters

##  MonadFail Sugar

## Polymorphization

## Bidirectional parsing

## Recursive go

