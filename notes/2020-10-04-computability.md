-- tag note computability
-- title computability
-- date 2020-10-04
-- source https://en.wikipedia.org/wiki/Turing_machine
          https://en.wikipedia.org/wiki/Computability_theory
;;
# Intro to computabilities

## Works on computable functions
- Kurt Godel: created general recursive functions.
- Alonzo Church: Created Lambda calculus. A function on natural numbers is called __λ-computable__ if it can be computed by lambda calculus
- Alan Turing: Craete Turning machine. A function is __turing computable__ if it can be computed by a turing machine

Later it's proved that these three systems are equivalent.

## Turing completeness
A computational model that can be implemented by any turing machine.


## Turing equivalence
Two computers P and Q are called equivalent if P can simulate Q and Q can simulate P.


## Church-Turing thesis (what is a computable function)
Any functions whoes value can be computed by an algorithm can be computed by a turing machine.

Formal version: A function on the natural numbers can be calculated by an effective method iff it's computable by a turing machine.


## General recursive functions --
General recursive functions are partial functions that take finite tuples of natural numbers and return a single natural number.

General recursive functions are the smallest class of partial funtions that includes the initial functions and is __closed under composition, primitive recursion and μ operator__.

#### Total function and parital functions
- Integer multiplication (a, b) = a * b is a __total function__ of ℤ² → ℤ, because all elements in it's domain will have a correspondence in the codomain.
- Integer division (a, b) = ⌊a / b⌋ is a __parital function__ of ℤ² ⇀ ℤ (Notice the half arrow), because not all elements in the domain has result in codomain. E.g (a / 0) is not defined.


#### Primitive recursive function (function that can be computed by for loops..)
Functions that can be computed with a known upper bound of the number of iteration before entering the loop.

- Most function we care in mathematics in general are primitive recursive functions (+, /, factorial, exp...)

PS: n-ary function means the function takes n parameters.


##### Definitions
Note primitive recursive functions are defined on natural numbers.

- Primitive recursive functions known __by axioms__
    - __1__. __Zero Constant function__ (zero): 0-ary functions `f() = 0`
    - __2__. __Successor fuction (succ)__ : 1-ary functions `S(k) = k + 1` where k ∈ ℕ (Peano natural numbers)
    - __3__. __Projection function__: `Pᵢ(x₁, x₂, ..., xₙ) = xᵢ` where i < n, i, n ∈ ℕ
- More complex primitive recursive functions
    - 4. __Composition (∘)__: For k-ary primitive recursive function f and k many m-ary primitive recursive function g₁ to gₖ, then `h(x₁, ..., xₘ) = f(g₁(x1, ..., xₘ), ..., gₖ(x1, ..., xm))`` is also primitive recursive.
    - 5. __Primitive recursive (ρ)__: let f be a k-ary primitive recursive function, and g be a (k+2)-ary primitive recursive function. the (k+1)-ary function h is defined as the primitive recursion of f and g. E.g the function h is primitive recursive when `h(0, x₁, x₂, ..., xₖ) = f(x₁, ..., xₖ)` and `h(succ(y), x₁, ..., xₖ) = g(y, h(y, x₁, ..., x), x₁, x₂, ..., xₖ)`

PS: __6. minimization__ is the property that makes primitive recursive function to general recursive function.

PS: Primitive recursive functions always halt. But general recursive functions doesn't because it supports minimization, which allow us to describe parital functions.

PS: __Composition__ is defined on a function and a set of functions that yields it's parameters.

PS: Projection functions is used to select argument of one function to other functions.
```
-- Primitive recurisive function.
-- Notice this is not a formal primitive recursive definition.
-- "Wiring" parameters is not part of the axioms
f(a, b, c) = g(h(c, a) , h(a, b))

-- formal primitive recursive definition with
-- components and projections:
f(a, b, c) = g(h(proj₃³(a, b, c), proj₁³(a, b, c)),
               h(proj₁³(a, b, c), proj₂³(a, b, c)))
```

PS: __Primitive recursion ρ__
```
Let's define
    f : ℕⁿ ⇀ ℕ, g : ℕⁿ⁺² → ℕ

    h = ρⁿ(f, g) : ℕⁿ⁺¹ → ℕ
    h(0, x₁, x₂, ..., xₖ) = f(x₁, ..., xₖ) -- Base case
    h(succ(y), x₁, ..., xₖ) =
        g(y, h(y, x₁, ..., x), x₁, x₂, ..., xₖ) -- Recursive step

-- intuition:

function h acts like a for loop from o to the value of it's first argument.

f is the base case function and it will only execute onece.

function g is the for loop body. it takes two more input than the base case f.
Namely the final input y to h and the recursive call h(y, x₁, ..., x).
g feed current information it needs for recursive step.

different functions might only need part of the argument, to achieve that
we use projection.
```

##### Some examples.
Note for primitive recursion different books will use different orders. We use this order:

```
-- Base case:
--     h(0, x₁, x₂, ..., xₖ) = f(x₁, ..., xₖ)
-- Recursive step:
--     h(succ(y), x₁, ..., xₖ) = g(y, h(y, x₁, ..., x), x₁, x₂, ..., xₖ)
-- where y is the final input,
--       h is the recursive call.
```

Some basic examples:
```
one : ℕ → ℕ
one = succ ∘ zero

four : ℕ → ℕ
four = succ ∘ succ ∘ succ ∘ succ ∘ zero
```

you can compose with multiple functions as long as the arity matches up.
```
plusFour : ℕ → ℕ
plusFour = add ∘ [proj₁¹, four]

-- using composition and projection to select argument.
addFirstAndThird : ℕ³ → ℕ
addFirstAndThird = add ∘ [proj₁³, proj₃³]
```

This uses primtive recursion. Something to be aware of:
- You can't just directly "wire" parameters around, instead you compose with the projection

```
-- define by match cases
add' : ℕ² → ℕ
add' (0, x) = x
add' (succ(n), x) = succ(proj₂³(n, add(n, x), x))

-- compact version with primitive recursion axiom.
add : ℕ² → ℕ
add = ρⁿ(proj₁¹, succ ∘ [proj₂³])

-- Another definition with primitive recursion
mult : ℕ² → ℕ
mult = ρ¹(zero, add ∘ [proj₂³, proj₃³])
-- note
-- first for base case f : ℕ → ℕ, mult(x, 0) = 0. so f = zero
-- then for recursive step, mult(x, y+1) = x + mult(x, y)
-- g : ℕ³ → ℕ = g(x₁, x₂, x₃) = g(y, x, mult(x, y))

-- predcessor
pred : ℕ → ℕ
pred = ρ₀(zero, proj₁²)

pred' : ℕ → ℕ
pred' (zero) = zero
pred' (succ(n)) = proj₁²(n, pred'(n))
```

#### Partial recursive function (general recursive or μ recursive function)
Primitive recursive function with __Minimization__. Close under composition, primitive recursion, and μ operator.

PS: __Minimization (μ operator)__

Without minimization we can only define total function. but with
minimization we can define partial functions.

```
f : ℕⁿ⁺¹ ⇀ ℕ
μⁿf : ℕⁿ ⇀ ℕ


μⁿf(x₁, ..., xₖ) = z ⇔ f(i, x₁, ..., xₖ) > 0 for i = 0, ..., z - 1
                       f(z, x₁, ..., xₖ) = 0

-- intuition
μ defines an unbounded search operator.

Minimization begin the search from i = 0 and proceeding upwards the
smallest argument that causes the function to return 0.

If there is no such argument, the function doesn't halt. And this is
way it's a partial function. Some inputs are undefined.

```

#### Primitive resursive functions ⊂ total recursive function.
- __partial recursive function__: Primitive recursive function that supports `μ operator` (Or functions thats can be comoputed by a turing machine)
- __total recursive function__: Partial recursive function that is defined for every input.

All primitive recursive functions are total function, but not all total functions are primitive recursive. The most well known example is the Ackermann functionA(m, n).

```
    +--total recursive------+
    | +-primitive+          |
    | |          |          |
    | |          | Ackermann|
    | |          |          |
    | +-recursive+          |
    +-----------------------+
```


#### General recursive function In programming language
Bounded for loop is primitive recursive function; while and goto makes partial recursive function. Algorithms can be expressed in these structures are computable because they are general recursive.


#### PS
- All primitive recursive functions halt. (It's not sure for arbitrary turing machine)
- Lots of type system are designed based on primitive recursive function to make sure they halt.

## Turing machine --
A conceptual machine provides a formal definition of algorithms.

A turing machine consists of:
- A tape divided into cells.
- A head that can read and write symbols on the tape, and move the tape left and right one cell at a time.
- A state register stores the state of the turing machine.
- A finite table of instructions.


#### Formal definition
Turing machine defined as 7-tuple.
```
M = ⟨Q, Γ, b, ∑, δ, q₀, F⟩ where
    Q:           finite, non empty __set of states__.
    Γ:           finite, non empty set of tape __alphabet symbols__.
    b ∈ Γ:       blank symbol (The only symbol allowed to occur on the tape infinitely)
    ∑ ⊆ Γ \ {b}: Set of input symbols allowed to appear in the inital tape content.
    q₀ ∈ Q:      initial state
    F ⊆ Q:       Set of final states.
                 (the initial tape is said to be accepted by M if it halts in a state ∈ F)
    δ:           (Q \ F) × Γ ⇀ Q × Γ × {L, R}, a partial function called transition function
                 where {L, R} represent left shift or right shift.
                 If δ is not defined on the current states and the current tape symbols, the
                 machine halts.
```

#### Busy beaver
Busy beaver game

State table for 3 state 2 symbol busy beaver defined by 7-tuple turing machine:
```
BusyBeaver = ⟨Q, Γ, b, ∑, δ, q₀, F⟩ where
    Q = {A, B, C, HALT}
    Γ = {0, 1}
    b = 0
    ∑ = {1}
    q₀ = A
    F = {HALT}
    δ = defined as the state table below

State Table for (Q \ F) × Γ ⇀ Q × Γ × {L, R}:
+=======+===========+===========+=============+
| Symbol|  State A  |  State B  |   State C   |
+=======+===========+===========+=============+
|   0   | (1, R, B) | (1, L, A) | (1, L, B)   |
+=======+===========+===========+=============+
|   1   | (1, L, C) | (1, R, B) | (1, R, HALT)|
+=======+===========+===========+=============+
```

This shows how do you define an "algorithm" with turing machine. The current state of the  will transform


#### The state
The state in a turing machine is referred to the entire state of the machine, including both the current instruction and the all the symbols on the tape. So the transition

## Lambda Calculus --

