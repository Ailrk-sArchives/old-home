-- tag math abstract-algebra
-- title More abstract algebra.
-- date 2020-08-05
-- source https://www.youtube.com/watch?v=xX-vBP3Yfuc&list=PL0vayjHV8kQCAll2Pb7JM4IOtLd6jPJMC&index=2&t=8s
;;
# More abstract algebra

## More proofs (yeah!)

### Prove a subset H is a subgroup of Group G.

#### statement:
```
    Prove H = 2ℤ is a subgroup of ℤ (H ≤ G)
```

#### note: prove subgroup ≡ prove these three properties.
```
    We say H ≤ G if
    1. H ≢ ∅
    2. H is closed under the group operation
        ∀ x, y ∈ H, we have xy ∈ H.
    3. H is closed under inverses
        ∀ x ∈ H, ∃ x ∈ H s.t. x⁻¹ ∈ H.
```

#### proof:
```
    1.  0 ∈ ℤ ⇔ 0 = 2 . 0 ∈ H ⇔ H ≢ ∅.
    2.  Say x, y ∈ H.
        this means x = 2n, y = 2m for some m, n ∈ ℤ.
        thus xy = 2n + 2m = 2(m + n)
        we know m + n ∈ ℤ
        ⇒ 2(m + n) ∈ 2ℤ ⇔ 2(m + n) ∈ H.
    3.  Say x ∈ H, x = 2n for some n ∈ ℤ.
        assume x⁻¹ exists.
        x⁻¹ + x = 1
        ⇔ x⁻¹ = 0 - 2n
        ⇔ x⁻¹ = -2n
        ⇔ x⁻¹ = 2(-n)
        because (-n) ∈ ℤ, 2(-n) ∈ 2ℤ ≡ H.
        thus x⁻¹ ∈ H.
    so H ≤ G
    ∎
```

### Proof the set of all elements of order 2 with the identity is a subgroup of an abelian group

#### statement
```
Let G be an abelian group and H = {x ∈ G | x² = e}
Prove H ≤ G.
```

#### proof:
```
    Suppose G is abelian.
    1. e² = ee = e ∈ H ⇒ H ≢ ∅.
    2. say a, b ∈ H.
       so {
           a² = e,
           b² = e.
       }
       note because abelian, (ab)² = abab = aabb = ee = e.
    3. say a ∈ H so a² = e.
       note (a⁻¹)² = a⁻² = (a²)⁻¹ = e⁻¹ = e.
    H ≤ G
    ∎
```

### Prove a cyclic group is actually a group

#### statement:
```
Let G be a group and H = ⟨a⟩ = {aⁿ | n ∈ ℤ},
where a ∈ G, prove H ≤ G.
```

#### proof:
```
    Let G be a group and H = ⟨a⟩ = {aⁿ | n ∈ ℤ}, a ∈ G
    1. If n = 0, a⁰ = e ∈ H ⇔ H ≢ ∅.
    2. say x, y ∈ H,
       then
       x = aⁿ
       y = aᵐ
       xy = aⁿaᵐ = aⁿᵐ
       because mn ∈ ℤ, aⁿᵐ ∈ H.
    3. say x ∈ H,
       then x = aⁿ
       e = aⁿ * a⁻ⁿ
       ∵ -n ∈ ℤ, a⁻ⁿ ∈ H
       so ∃ x⁻¹ = a⁻ⁿ
    ∎
```

### Prove the cenralizer is a subgroup.

#### statement:
```
Let G be a group and fix g ∈ G.
prove Z(g) = {x|gx = xg} ≤ G.
      (Centralizer)
```

#### proof:
```
    Let G be a group and fix g ∈ G.
    1. note ge = g = eg ⇒ e ∈ Z(g) ⇒ Z(g) ≡ ∅.
    2. take x, y ∈ Z(g)
       gx = xg
       gy = gy
       (xy)g = x(yg) = x(gy) = (xg)y = (gx)y = g(xy)
       so x, y ∈ ℤ
    3. take x ∈ Z(g)
       gx = xg
       x⁻¹g = x⁻¹ge = x⁻¹g(xx⁻¹) = x⁻¹(gx)x⁻¹
       = x⁻¹(xg)x⁻¹ = gx⁻¹
       x⁻¹ ∈ Z(g)
    Z(g) ≤ G
    ∎
```
