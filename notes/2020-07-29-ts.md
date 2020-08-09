-- tag note typescript todaysRandom
-- title Note: Some more typescripts
-- date 2020-07-29
-- source https://www.typescriptlang.org/docs/handbook/utility-types.html
          https://www.typescriptlang.org/docs/handbook/advanced-types.html#interfaces-vs-type-aliases
;;
# Some typescript
##### 1. Typescript doesn't exist at runtime.
Runtime of typescript is defined by js standard, so typescript has no control over it. Because of this some features can only exist in compile time.
__1__. Interface can not exist in runtime, thus you can not have default implementation for interface. To work around one needs to extend from another concrete class which implemented the interface.
__2__. You need type guard to check type's identity; there is even a dedicated syntax to write type guard. This could be eliminated withh RTTI.
__3__. Idiomatic way of doing runtime type checking is using string comparison based on `typeof` or tagged union.

##### 2. Typescript try not to add too many crazy features on top of js.
This is more of a personal opinion. `namespace` is now marked as deprecated even though the way how it is compiled is very clearly defined, and typescript itself heavly relied on it. I was thinking could it introduce a more ergonomic way of writing algebraic data type, like compile the adt's name as the `kind` property in current tagged union. But it seems too much work on top of js and will never happen.

##### 3. want metaprogramming just use babel.
Neither js nor ts has good language level support for meta programming. Compare with python which you can almost override all syntax in the language, you can do very little on typescript. But this is not a problem if you can modified the AST directly. Typescript and javascript today generally get compiled through babel, which is a perfect place to perform code transformation.

##### 4. Type parameter can be more restrict than type they extends from.
Extends can be read as can assign to. The example below `const a: boolean = true as True` is valid because of structual typing, so `True` a valid candidate for T. But false is not assignable to true, so the default parameter doesn't work here.
```typescript
interface OnlyBool<T> {
  (arg: T): T;
}
functor onlyBool<T extends boolean>(arg: T = false): T {
  return arg;
}
type True = true;
// True extends boolean, but false cannot assign to it.
```

##### 5. type equality
In a sound type system two type are equal if they are subtpe of each other. But in typescript `any` violate the law since it is both subtype and supertype of all types. So an implementation like `type<A, B> = A extend B ? B extend A ? true : false : false` doesn't cover all possibilities.
Better equality check:
```typescript
  export type IfEqual<T, U> =
    (<G>() => G extends T ? 1 : 2) extends
    (<G>() => G extends U ? 1 : 2) ? true : false;
```
It doesn't check on type directly but rather extend the comparison between 1 and 2.

##### 6. Type family in typescript (express a type depends on other type).
Type alias plus conditional really gives you indexed type. You

##### 7. Extract type from a value
This is very limited. It can either done my using `typeof` or type guard. `typeof` is convinent but it's restricted among primitives only.
```typescript
declare const a: number;
declare const x : typeof a;
declare functor f(x): typeof x;
```

##### 8. Convert value to literal type
Infer by branch. This mechanism reflect the branch on term level to type level.
```typescript
const x: boolean =  getBool();
if (x === true) { const a = x; }  // x :: true
else { const b = x; }  // y :: false
```

##### 9. Use literal type in term level
In `declare const a = IsEqual<"a", "a">;` a can only be true, so even it didn't get assigned with a value ts know it is a true. Similarly, `declare const b = IsEqual<"a", "a" extend true ? "string1" : "string2">` can only be `string1`.
The problem is this types only exist at type level. But since there is only one possible value for them, is there a way to extract it into term level? (I can think of using babel to replace the value directly. If it works it gonna be dependent type in typescript)

##### 10. wierd typescript's interface overload.
Normally you think if you have overload you implement different versions of a function. But in typescript you define a function that accept all possible parameter of all overloads, and the only thing typescript does is to generate type signatures for each "overload".
```typescript
interface I{
  foo(a: number): number,
  foo(b: string, c: number): string,
}
```
To implement this interface you need to declare both signature again, and add a "real implementation"
```typescript
class A implement I {
  foo(a: number): number,
  foo(a: string, b: number): string,
  foo(a: number | string, b? number): number | string {
    return "1";
  }
}
```
So overload in typescript really is just used to establish the correlation between parameter types and return types. You don't have c++ style overload that treat each overload as different functions.


##### 11. today's random.
I wanted to have a function works like this.
```typescript
get<"default">(1) // -> Map<number, M>
get<"time">(new Date().toJSON()) // -> Map<number, Array<M>>
get<"tag">("t") // -> Map<number, Array<M>>
```
It is possible if we can convnert this (`declare const b = IsEqual<"a", "a" extend true ? "string1" : "string2">`) from type level to value.
