-- tag math abstract-algebra
-- title More abstract algebra. Proofs and subgroups.
-- date 2020-08-04
-- source https://www.youtube.com/watch?v=xX-vBP3Yfuc&list=PL0vayjHV8kQCAll2Pb7JM4IOtLd6jPJMC&index=2&t=8s
;;
# More abstract algebra

## Even more proof

### Prove (ℝ+, +) is not cyclic
#### statement:
```
    (ℝ+, +) is not cyclic
```

#### proof by contradiction:
```
    Suppose (ℝ+, +) is cyclic. Then ∃ x ∈ ℝ s.t ℝ = ⟨x⟩.
    so
    ℝ = ⟨x⟩ = { n * x | n ∈ ℤ } ... ₁

    On the other hand, arbitrarily choose 0.5 ∈ ℝ, we konow 0.5x ∈ ℝ.
    assume ₁ holds, then
    0.5x = nx for some n ∈ ℤ.
    note x ≢ 0, becuase if so we have ⟨x⟩ = {0} ≡ ℝ.
    thus
    0.5x = nx ⇔ 0.5 = n.

    But n ∈ ℤ, contradict.

    Thus (ℝ+, +) is not cyclic
    ∎
```

### Prove (ℤ×ℤ, +) is not cyclic
#### statement:
```
    (ℤ×ℤ, +) is not cyclic
```

#### side note:
```
    (ℤ, +) = ⟨1⟩
```

#### proof by contradiction again...:
```
    Suppose that (ℤ×ℤ, +) is cyclic, then
    ∃ a, b ∈ ℤ s.t ℤ×ℤ = ⟨(a, b)⟩

    we know (1, 1) ∈ ℤ×ℤ
    ⇔ ∃ m ∈ ℤ s.t. (1, 1) = m (a, b) = (ma, mb)
    ⇔ {
        1 = ma, ...₁
        1 = mb,
    }

    we know (0, 1) ∈ ℤ×ℤ
    ⇔ ∃ n ∈ ℤ s.t. (0, 1) = n (a, b) = (na, nb)
    ⇔ {
        0 = na,
        1 = nb, ...₂
    }

    na = 0 ⇒ n = 0 ∨ a = 0.
    if n = 0, by ₂, 1 = 0 . b = 0. absurd.
    if a = 0, by ₁, 1 = m . 0 = 0. absurd.

    So na ≠ 0, contradict.

    Thus (ℤ×ℤ, +) is not cyclic
    ∎
```


### Prove every cyclic group is abelian
#### statement:
```
    Every cyclic group is abelian.
```

#### proof:
```
    Suppose G = ⟨x⟩ is cyclic.
    ∃ a, b ∈ G s.t. a = xⁿ, b = xᵐ for some n, m ∈ ℤ.
    then
        ab = xⁿxᵐ = xⁿ⁺ᵐ = xᵐ⁺ⁿ = xᵐxⁿ = ba
    ∎
```


## Subgroup
#### define:
```
Let (G, *) be a group. A subgroup of (G, *) is a subset H ⊆ G s.t.
H is also a group under the same operation *.
```

#### example-1
```
G = ℤ, + = "addition".
H = 3ℤ = { 3n | n ∈ ℤ }, + = "addition".
H is a subgroup of G.
```

#### example-2
```
G is a group, {e} and G are both subgroup og G.
```

#### example-3
```
G is a group and x ∈ G.
then ⟨x⟩ = {xⁿ | n ∈ ℤ} is a subgroup of G.
```

#### example-4
```
Additive group of integer modulo 4.
ℤ₄ = {0, 1, 2, 3}
⟨2⟩ = {0, 2}
// it's a proper subgroup.
```

#### example-5
```
Let G be a group.
Let Z(G) = {x ∈ G | xg = gx ∀ g ∈ G}
where Z is called `center of G`
Z(G) ≤ G.
```

#### example-6
```
GL: General Linear group
SL: Special Linear group
G = GL(n, ℂ) = { all n×n complex matrices with non zero determinant }
H = SL(n, ℂ) = { all n×n complex matrices with determinant 1 }
H ≤ G
```
