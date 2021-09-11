-- tag lambda-calculus
-- title Prove Some Properties of STLC
-- time 2021-05-04
-- source https://www.cs.uoregon.edu/research/summerschool/summer16/notes/AhmedLR.pdf

;;
## Prove Some Properties of STLC
- STLC add types on top of lambda calculus.
- Introduced to avod paradox of intyped lambda calculus (Y combinator cause recurison)
- We can add more constructs to enrich basic structures of STLC. e.g adding products.

### Setup notations

##### Simply typed lambda calculus
Define some notations to use.
Use STLC with builtin boolean values, this give us someting to prove.

The basic language is defined as follow

```
# Type
  τ := bool | τ → τ

# Constants
  c := true | false

# Expression
  e := x | λx:τ.e: | e e
      | c
      | if e then e else e

# Context
  Γ := ∙ | Γ, x:τ
```

###### Semantics

```
# Application
  (λx:τ . e) v → e[v/x]

# If else
  if true then e₁ else e₂ → e₁
  if false then e₁ else e₂ → e₂
```


###### Some typing rules for STLC

```
# T-False
# axiom: we have boolean value false

  -------------
   Γ⊢false:bool

# T-True (false is false)
# axiom: we have boolean value true

  -------------
   Γ⊢false:bool

# T-Var
# if x maps to τ in the context Γ, then variable x has type τ

  Γ(x)=τ
  ------
  Γ⊢x:τ

# T-Abs
# abstraction rule. Given variable x with type τ₁ implies an expression
# with type τ₂, we have the lambda term λx.e with type τ₁ → τ₂

    Γ, x:τ₁ ⊢ e:τ₂
   ------------------------
   Γ ⊢ (λx:τ₁ . e):τ₂→τ₂

# T-App
# Application rule. Given a function e₁ : τ₁ → τ₂ and a variable e₂ : τ₁,
# we can apply e₁ to e₂ to get a new term e₁ e₂ : τ

    Γ ⊢ e₁:τ₁→τ₂    Γ ⊢ e₂:τ₁
  ---------------------------
      Γ ⊢ (e₁ e₂):τ


# T-If
# If can be simulated with church encoded boolean values easily, we
# simply add it as a builtin construct.

    Γ ⊢ e₁:bool  Γ ⊢ e₂:τ  Γ → e₃:τ
  -----------------------------------
    Γ ⊢ (if e₁ then e₂ else e₃):τ


```


### 1. STLC programs terminate

### 2. programs are type safe

