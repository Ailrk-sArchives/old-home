-- tag rust gdb cargo haskell
-- title Note: Some typescripts, some rust, some haskell
-- date 2020-07-27
-- source https://www.typescriptlang.org/docs/handbook/utility-types.html
          https://www.typescriptlang.org/docs/handbook/advanced-types.html#interfaces-vs-type-aliases
;;
# types for typescripts.
1. structual typing
    Types are equivalence if they have the same shape. As it opposes to nominative tying.
    Structual typing can model js's objects very well, namly you can write similar
    duck typing code as in js but type checked.

2. tagged union
    How algebraic data type is called in ts.
    for example:
    ```typescript
    interface Fish {
      kind: "fish",
      scaleColor: string,
    }
    interface Bird {
      kind: "bird",
      featherColor: string,
    }
    interface Animals = Fish | Bird;
    ```
    and you match type with property `kind`. `kind` kinda works like data constructor in haskell (not!)

3. some special type level operations
    typescript has some very specific type level operations like index types and conditionals.

3. `keyof` to get make union type.


