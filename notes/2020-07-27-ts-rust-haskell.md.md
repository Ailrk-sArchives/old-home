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

3. `keyof` to get make union type.
    ```typescript
    type A<T> = keyof T;
    ```
    will give you union of literal types of keys of T.

4. map types.
    `type` can be thought as declaring a type level function.  so `type A<T> = keyof T` take type parameter `T`
    and return  a new type `keyof T`
    Mapping of types unlock a lot of type level operations.

5. `in` is a overloaded.
    1. type level`in` select each member of union type
    2. term level `for .. in` iter overall enumerable properties, `in` alone is a membership test.

6. `for .. of` and `for .. in`?
    1. `for .. in` is for enumerable properties. Using it to iter over object properties.
    2. `for .. of` is for iter over iterable objects. e.g `Array`, `String`, `TypedArray`, `Map` and `Set`.

7. type aliase can only be refered recursively in a property.
    This works.
    ```typescript
    type Tree<T> = {val: T, left: Tree<T>, right: Tree<T>}
    ```
    But this doesn't
    ```typescript
    type Yikes = Array<Yikes>
    ```
    Yikes!

8. Mapped types
    ```typescript
    type Id<T> = { [P in keyof T]: T[P] }
    ```
    It has some pattern mathcingish features like
    ```typescript
    type Readonly<T> = { readonly [P in keyof T]: T[P] }
    type Partial<T> = { [P in keyof T]?: T[P] }
    ```

9. conditional types
    Write some logics in type level.
    first, `A extends B` means A is safely assignable to B.
    `A extends B ? X : Y` ≡ A ⊆ B → X ∧ A ⊊ B → Y

10. never type
    a function has nevr return type never return. Or in another word this type is not
    reachable.

11. distributivie conditional types
    suppose you have `type T = A | B | C`,  `T extends U ? X : Y` will distributed in to type
    `A extends U ? X : Y | B extends U ? X : Y | C extends U ? X : Y`
    It's like map conditional over union types.

12. type inference in conditional types.
    `infer` declaration introduce a type variable to be inferred.
    ```typescript
    type ReturnType<T> = T extends (..args: any[]) => infer R ? R : any;
    ```

13. pattern matching on conditinoals
    ```typescript
    type Unpacked<T> =
      T extends (infer U)[] ? U :
      T extends (...args: any[]) => infer U ? U :
      T extends Promise<infer U> ? U :
      T;

    // multiple candidates for the same type variable in co-variant position
    // causes a union type to be inferred.
    type Foo<T> = T extends { a: infer R, b: infer R} ? R : never;
    Foo<{a: string, b: number}> //  'string' | 'number'

    // intersection type is inferred for mulitple candidates for the same variable in
    // contra-varaint position
    type Bar<T> = T extends { a: (x: infer U) => void, b: (x: infer U) => void} ? U never;
    Bar<{ a: (x: string) => void, b: (x: number) => void }> // string & number
    ```

14. conclusion
    Typescript use structual typing. You can use type aliase to make type level function to
    make between types. interface is like record type with the property name as accessor.
    abstract data types are achieved by tagged union, and you can use conditionals to map types
    non uniformly.
