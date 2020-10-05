-- tag note computability
-- title computability
-- date 2020-10-04
-- source
;;
# Intro to computabilities


## Computable functions.


## Works on computable functions
- Kurt Godel: created general recursive functions.
- Alonzo Church: Created Lambda calculus. A function on natural numbers is called __λ-computable__ if it can be computed by lambda calculus
- Alan Turing: Craete Turning machine. A function is __turing computable__ if it can be computed by a turing machine

Later it's proved that this three systems are equivalent.

## Turing completeness
A computational model that can be implemented by any turing machine.


## Turing equivalence
Two computers P and Q are called equivalent if P can simulate Q and Q can simulate P.


## Church-Turing thesis (What is computable)
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
    - __2__. __Successor fuction (μ, or succ)__ : 1-ary functions `S(k) = k + 1` where k ∈ ℕ (Peano natural numbers)
    - __3__. __Projection function__: `Pᵢ(x₁, x₂ ... xₙ) = xᵢ` where i < n, i, n ∈ ℕ
- More complex primitive recursive functions
    - 4. __Composition (∘)__: For k-ary primitive recursive function f and k many m-ary primitive recursive function g₁ to gₖ, then `h(x₁ ... xₘ) = f(g₁(x1 ... xₘ), ... gₖ(x1 ... xm))`` is also primitive recursive.
    - 5. __Primitive recursive (ρ)__

PS: Composition is a general case, and it can be specialized to h(x) = f(g(x)).

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

##### Some examples.

```
one : ℕ → ℕ
one = succ ∘ zero

four : ℕ → ℕ
four = succ ∘ succ ∘ succ ∘ succ ∘ zero

add : ℕ² -> ℕ
add = proj₁¹
add succ(n) x = succ(proj₂³(n, add(n, x), x))

-- you can compose with multiple functions as long as the
-- arity mathes up.
plusFour : ℕ → ℕ
plusFour = add ∘ [proj₁¹, four]

```

#### Basic functions


## Turing machine --



## Lambda Calculus --

