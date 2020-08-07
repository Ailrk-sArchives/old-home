-- tag math abstract-algebra
-- title More abstract algebra.
-- date 2020-08-06
-- source https://www.youtube.com/watch?v=xX-vBP3Yfuc&list=PL0vayjHV8kQCAll2Pb7JM4IOtLd6jPJMC&index=2&t=8s
;;
# Today's abstract algebra

## General linear group
#### define
```
    GL(n, ℝ)
    = {all n×n matrices with nonzero determinant and real entries}
    = {all n×n intertable matrices with real entries}
```

## Special linear group
#### define
```
    SL(n, ℝ) =
        {all matrices with determinant equal to 1 and real entries}
```

## Proof
### prove the special linear group is a subgroup of the general linear group.

#### statement
```
SL(n, ℝ) ≤ GL(n, ℝ)
```

#### proof
```
    Suppose H = SL(n, ℝ)
            G = GL(n, ℝ)

    1. note I = |1 0|
                |0 1|
       det(I) = 1, so I ∈ H. ⇒ I ≢ ∅
    2. take A, B ∈ SL(n, ℝ)
       so
       det(A) = 1, det(B) = 1.
       det(AB) = det(A)det(B) = 1 . 1 = 1
       ⇒ AB ∈ SL(n, ℝ)
    3. take A ∈ SL(n, ℝ)
       so det(A) = 1
       det(A⁻¹) = (det(A))⁻¹ = 1⁻¹ = 1
       ⇒ A ∈ SL(n, ℝ)
    so SL(n, ℝ) is a group
    ∵ SL(n, ℝ) ≤ GL(n, ℝ)
```

### Prove a finite nonempty subset of G closed under the group operation is a subgroup
#### statement:
```
Supose G is a group and H is a finite nonempty subset of G which
is closed under the group operation.
Prove H ≤ G.
```

#### proof:
```
    1. H ≢ ∅ by defition.
    2. H is closed under group operation by definition.
    3. let x ∈ H.
       note x², x³, x⁴, ... ∈ H,
       but H is finite, so ∃ m, n ∈ ℤ, m ≢ n s.t. xᵐ = xⁿ.
       Without lost of generality, assume m > n.
       Note xᵐ⁻ⁿ = e, so xᵐ⁻ⁿ⁻¹ = x⁻¹.

       Since m > n, m - n > 0.
       So m - n ≥ 1.

       if m - n = 1: x⁻¹ = x¹⁻¹ = e ∈ H.
       if m 0 n > 1:
           xᵐ⁻ⁿ⁻¹ ∈ x², x³, x⁴, ... ∈ H.
       in both cases x⁻¹ ∈ H.
       So H is closed under inverse.
    so H ≤ G
    ∎
```

### Prove the intersection of two subgroups is also a group
#### statement:
```
If G is a group and H, K ≤ G, then H ∩ K ≤ G.
```

#### proof:
```
    Suppose H, K ≤ G.
    1. since H, K ≤ G, H ≢ ∅ ∧ G ≢ ∅.
       thus H ∩ K ≢ ∅.
    2. let x, y ∈ H ∩ K.
       thus x, y ∈ H ∧ x, y ∈ K.
       since H close under group operation, xy ∈ H
       since K close under group operation, xy ∈ K
       xy ∈ H ∩ K
    3. x ∈ H ∩ K.
       x ∈ H ∧ x ∈ K.
       because H, K close undere inverse
       x⁻¹ ∈ H ∧ x⁻¹ ⇔ K ⇔ x⁻¹ ∈ H ∩ K
    so H ∩ K ∈ G
    ∎
```

### Proof. the center of a subgroup of GL(2, ℂ)
#### statement:
```
G = GL(2, ℂ),
H =
  { |a b|       |        }
  { |c d|  ∈ G  |  c = 0 },
B =
  { |a b|       |                    }
  { |0 d|  ∈ H  |  a = d ≢ 0 ∧ b = 0 }

prove Z(H) = B
```

#### proof:
```
```
