-- tag abstract-algebra haskell
-- title abstract algebra. Order of an element, cyclic group.
-- date 2020-08-01
-- source https://www.youtube.com/watch?v=xX-vBP3Yfuc&list=PL0vayjHV8kQCAll2Pb7JM4IOtLd6jPJMC&index=2&t=8s
;;
# Abstract algebra and some haskell

## Some more proof

#### Proof of the formula for the inverse of a product.
##### statement:
```
If G is a group, and x, y ∈ G, then (xy)⁻¹ = y⁻¹x⁻¹
```

##### proof:
```
Suppose G is a group, x, y ∈ G. So x⁻¹, y⁻¹ ∈ G.
case 1:
    (xy)(y⁻¹x⁻¹)
  = x(y(y⁻¹x⁻¹)
  = x(yy⁻¹x⁻¹)
  = x(ex⁻¹)
  = xx⁻¹
  = e
case 2:
    (y⁻¹x⁻¹)(xy)
  = y⁻¹(x⁻¹(xy))
  = y⁻¹(x⁻¹xy)
  = y⁻¹(ey)
  = y⁻¹y
  e
thus
  (xy)⁻¹ = (y⁻¹x⁻¹)
  ∎
```

#### Proof if x² = e ∀ x ∈ G then G is an abelian group
##### statement:
```
If G is a gorup s.t ∀ x ∈ G, x² = e then  G is abelian.
```

##### proof:
```
x² = e ⇒ x = x⁻¹.
xy = (xy)⁻¹ = y⁻¹x⁻¹ = yx
∎
```

## The order of an element.
#### define:
```
If G is a group, and g ∈ G, then we say g has finite order if ∃ n ∈ ℤ s.t gⁿ = e. If such an n exists, the smallest such n is called order of g, denoted o(g).
If g does not have finite order, we say that g has infinite order, denoted o(g) = ∞.
```

#### example-1
```
G = ℚ* = {non zero rationals}, * = multiplication.
-1 ∈ ℚ* has finite order because (-1)²ⁿ = 1 where n ∈ ℤ.
  o(-1) = 2 becase 2 is the smallest postive integer for (-1)² = e.
  o(3) = ∞
```

#### example-2
```
G = ℤ₂₁, ⊞ = addition mod 21
6 ∈ ℤ₂₁, 21 . 6 = 6 ⊞ 6 ⊞ 6 ⊞ ... ⊞ 6 = 0 => 6 has finite order.
note 7 . 6 = 0, => o(6) = 7
```
Side note
```
gⁿ = e for multiplication, n . g = e for addition
```

## Proof
#### Proof the order of x is the order of x inverse. ✭
##### statement:
```
Let G be a group and x ∈ G, prove o(x) = o(x⁻¹)
```

##### proof:
```
case 1: x has finite order.
  let n = o(x), then xⁿ = e.
    ⇒ (x⁻¹)ⁿ = x⁻ⁿ = (xⁿ)⁻¹ = e⁻¹ = e.
    ⇒ n is an integer that satisfy (x⁻¹)ⁿ = e.
    ⇒ o(x⁻¹) is finite and o(x⁻¹) ≤ o(x) = n.

    more formally,
    o(x) < ∞ ⇒ {
        o(x⁻¹) < ∞,
        o(x⁻¹) ≤ o(x) = n. ...₁
    }.

  ∵ x⁻¹ has finite order, apply the above to x⁻¹.
  ⇔ o((x⁻¹)⁻¹) ≤ o(x⁻¹)
  ⇔ o(x) ≤ o(x⁻¹)
  ∵ ₁,
  ⇔ o(x) = o(x⁻¹).

case 2: x has infinite order.
  claim o(x) = ∞ ⇒ o(x⁻¹) = ∞.
  This is the contrapositive of o(x⁻¹) < ∞ ⇒ o(x) < ∞,
  which is shown above.
 ∎
```

#### Proof if gⁿ=e then the order of g divides n.
##### statement:
```
Let G be a group, g ∈ G, n ∈ ℕ. Prove that if gⁿ = e, then
o(g) divides n. (o(g) | n)
```

##### proof:
```
Suppose gⁿ = e, so g has finite order, say m = o(g).
By division algorithm, ∃ r, p ∈ ℤ s.t
  n = mp + r where 0 ≤ r < m.

Note, e = gⁿ = gᵐᵖ+ʳ = gᵐᵖ.gʳ = (gᵐ)ᵖ.gʳ = eᵖ.gʳ = e.gʳ = gʳ.
So, gʳ = e.

But m is the smallest postive integer s.t gᵐ = e and r ≤ m.
Thus r = 0.
⇔ n = mp
⇔ m | n
⇔ o(g) | n
∎
```

## Cyclic Groups
#### notation
```
If G is a group, and x ∈ G, then <x> denotes all powers of x, namely
<x> = {xⁿ | n ∈ ℤ }.
If G is written additively,
<x> = {nx | n ∈ ℤ }.
```

#### define
```
A group G is cyclic if ∃ x ∈ G s.t G = <X> and in this case x is called a generator of G.
```

#### example-1
```
n ∈ ℕ, G = ℤₙ ⊞ = "addition modulo n"
1 is generator
because 1 ⊞ 1 ⊞ ..(n copies).. ⊞  1 = 0
ℤₙ = <1>
```

#### example-2
```
G = ℤ₅ ⊞ = "addition modulo n"
ℤ₅ = { 0, 1, 2, 3, 4, 5 }
3 ∈ ℤ₅
0 ⊞ 3 = 0
3 = 3
3 ⊞ 3 = 1
3 ⊞ 3 ⊞ 3 = 4
3 ⊞ 3 ⊞ 3 ⊞ 3 = 2
ℤ₅ = <3>
Note also we have <1>. Because ℤ₅ ⊂ { ℤₙ | n ∈ ℤ }
```

## Proof
#### Proof if x is a generator so is its inverse
##### statement:
```
if G = <x> then G = <x⁻¹>
```

##### proof:
```
Suppse G = <x>. Take any g ∈ G,
then since G=<x>, ∃ n ∈ ℤ s.t. g = xⁿ.
then g⁻¹ = (xⁿ)⁻¹ = x⁻ⁿ = (x⁻¹)ⁿ ∈ <x⁻¹>.

since <x⁻¹>  is closd under inverses ∧ g⁻¹ ∈ <x⁻¹>,
then g = (g⁻¹)⁻¹ ∈ <x⁻¹>.

so G ⊆ <x⁻¹>.
∵ <x⁻¹> ⊆ G
G = <x⁻¹>
∎
```
