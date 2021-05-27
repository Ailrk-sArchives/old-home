-- tag programming-languages
-- title A little Survey on Polymorphism
-- time 2021-03-01
-- source https://link.springer.com/content/pdf/10.1007%2F3-540-57880-3_16.pdf
          https://arxiv.org/pdf/1311.7203.pdf
          https://caml.inria.fr/pub/papers/xleroy-modular_modules-jfp.pdf
          https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/milner-type-polymorphism.pdf
          https://dl.acm.org/doi/10.1145/1449955.1449808
          https://catonmat.net/cpp-polymorphism
          https://wiki.haskell.org/Existential_type
          https://wiki.haskell.org/GADTs_for_dummies
          https://stackoverflow.com/questions/2354210/can-a-class-member-function-template-be-virtual#:~:text=Templates%20are%20all%20about%20the,to%20call%20at%20run%2Dtime.&text=Therefore%20you%20cannot%20have%20virtual%20member%20function%20templates.
;;
## A little survey on Polymorphism

<br/>

The most dreadful thing when learning a new programming language might be learning their polymorphism mechanisms: Every language has a new name for it's own system, but under the hood they solve roughly the same problem. For example, somebody might tell you that traits in Rust is the same as typeclass in Haskell, they both allow you to do ad hoc polymorphism while imposing constraints at the type level. But once you look into it, they're not quite the same. Haskell support multi parameter typeclass, which allows you to overload functions on multiple types; if you look at the syntax of Rust's trait, the first parameter is fixed to refer the type itself, it's hard to imagine how to extend it to dispatch on multiple parameters. There is a lack of universal terms to describe common features, and this is not only a burden for language learners, this makes it very difficult to describe ideas among other programmers with different backgrounds. This article is a little summary of polymorphism mechanisms that different languages adpot.

Before we look at concrete examples, it's worth to define what does polymorphism means. The most basic notion is to have the same interface works on different types.

- polymorphic function to invoke on different types. that's it.
- the effect is not rigirously defined. It can be the same function does completely different things for two types, or the same function works over different types that does the same thing.
- With typeclass constraint on type parameter, we have adhoc polymorphism on parametric polymorphism.


// TODO

In lambda cubes polymorphism is described as types-to-terms abstraction, meaning the term value depends on the given type.


The introduction of polymorphism comes from the realization that many


It's really just mean the ability to call the same function on different types, and inokve different functionalities.


If you ever write some C code, or use a C library, you will notice a lot of functions that are written in a "prefixed" style.


- Starting with a specfic efficient solution and, whenever possible, relaxing the requirements - is at the heart of generic programming.
- Polymorphism present in a program is a natural outgrowth of the primitive polymorphic operators which appear to exist in every programming languages.


### Subtyping polymorphism
Subtyping polymorphism is the most common polymorphism that can be found in most oop languages, and for many people this is what polymorphism means. The idea is you have some types, and it has super types which share the same behavior, but works differently. At call site, the language should be able to determine which

###### Single Dispatching
Late binding and subtyping polymorphism

```c++
class A {
public:
  virtual void foo() {
    std::cout << "A" << std::endl;
  }
};


class B : public A {
public:
  virtual void foo() override {
    std::cout << "B" << std::endl;
  }
};

int main() {
  A a1 = new A;
  B b1 = new B;
  A a1 = new B;

  a->foo();  // "A"
  b1->foo(); // "B"
  a1->foo(); // "B"
}
```

- implementation detail (vtable)

- note for c++

```c++
// this DOESN't work in current c++.
// the reason is not because it can't be done, but because
// the popular vtable implmenetation of dynamic dispatching
// doesn't work wit it.

class A {
  template<typename T> virtual void foo();
};
```


###### Generic function based mutl dispatch

CLOS style multiple dispatch


- Using common lisp to implement the same single dispatch
```lisp
(defclass a () ())
(defclass b (a) ())

(defgeneric foo (o)
  (:method (o a)
    (format t "a"))
  (:method (o b)
    (format t "b")))
```

- An example of multiple dispatch
This allows you to dispatch a function based on multiple types. What it really allow you to do is Haskell style multi paramter typeclass.

Because it's still dynamic disptaching, this will executed at runtime.

```lisp
(defclass a () ())
(defclass b () ())

(defgeneric (u v) (:document "u v"))

(defmethod foo ((u a) (v a)) (format t "a a"))
(defmethod foo ((u a) (v b)) (format t "a b"))
(defmethod foo ((u b) (v b)) (format t "b b"))
(defmethod foo ((u b) (v a)) (format t "b a"))
```

###### Simulate multiple dispatch in python

```python
registry = {}


class MultiMethod:
    def __init__(self, name):
        self.name = name
        self.typemap = {}

    def __call__(self, *args):
        types = tuple(arg.__class__ for arg in args)
        function = self.typemap.get(types)
        iffunction is None:
            raise TypeError("no match")
        return function(*args)

    def register(self, types, function):
        if types in self.typemap:
            raise TypeError("duplicate registration")
        self.typemap[types] = function


def multimethod(*types):
    def register(fn):
        name = fn.__name__
        mm = registry.get(name)
        if mm is None:
            mm = registry[name] = MultiMethod(name)
        mm.register(types, fn)
        return mm
    return register


@multimethod(int, int)
def foo(a, b=10):
    print("int int")


@multimethod(int)
def foo(a):
    print("int")
```

###### Simulate subtyping with existential type in haskell

```haskell
{-# ExistentialQuantification #-}
class Animal_ a where
  move :: a -> IO ()

data Animal = forall a. Animal_ a => Animal a
instance Animal_ Animal where
  move (Animal a) = move a

class Animal_ a => Cat_ a where
  meow :: a -> IO ()

data Cat = forall a. Cat_ a => Cat a
instance Animal_ Cat where
  move (Cat a) = move a
instance Cat_ Cat where
  meow (Cat a) = meow a

class Animal_ a => Dog_ a where
  bark :: a -> IO ()
data Dog = forall a. Dog_ a => Dog a

instance Animal_ Dog where
  move (Dog a) = move a
instance Dog_ Dog where
  bark (Dog a) = bark a

data Persian = Persian
instance Animal_ Persian where
  move _ = putStrLn "Persian the cat: Moving"
instance Cat_ Persian where
  meow _ = putStrLn "Persian the cat: Meow"

data Corgi = Corgi
instance Animal_ Corgi where
  move _ = putStrLn "Corgi the dog: Moving"
instance Dog_ Corgi where
  bark _ = putStrLn "Corgi the dog: woof woof"

persian = Persian
corgi1 = Corgi
corgi2 = Corgi

moveAnimals = sequence_ $ move <$>  [Animal persian, Animal corgi1, Animal corgi2]
moveDogs = sequence_ $ bark <$> [Dog corgi1, Dog corgi2]
```


### Parametric polymorphism

- predicative polymorphism (Martin Lof)
- impredicateive polymorphism (first class polymorphism)
- Bounded parametric polymorphsm

C++
Template
```c++
template <typename A, typename B>
A snd (std::pair<A, B> pair) {
  return std::get<0>(pair);
}
```

Higher kinded type with template template parameter.

```c++
#include<type_traits>
template <template <typename ...> typename F, typename ...Ts>
struct apply {
  using type = F<Ts...>::type;
};

using out_type = apply<head, int, double, char>::type

static_assert(std::is_same_v<our_type, int>);
```

Haskell without any extensions
- hindly minler
- algorithm w

```haskell
snd :: (a, b) -> a
snd (a, b) = a

let v1 = snd (1, 2) :: Integer
let v2 = snd ('a', "asd") :: Char
```

Haskell
```haskell
data Tree a = Branch a (Tree a) (Tree a)
```
- higher ranked.
- first class polymorphism with rank N type.
- higher ranked polymorphism.
- problem with rank N type.

Haskell
- GADT
- match return type on type constructor.
- GADT as special case of type family

```haskell
```


```c++
tempalte<typename T>
class Tree {
  T value;
  std::unique_ptr<Tree<T>> left;
  std::unique_ptr<Tree<T>> right;

  Tree(T&& value, std::unique_ptr<Tree<T>> left, std::unique_ptr<Tree<T>> right)
    : value(value)
    , left(std::move(left))
    , right(std::move(right)) {}
};
```

### Adhoc polymorphism

- Adhoc polymorphism allows the dynamic semantics of a program to be affected by the types of values in that program.
- A program may have more than one type derivation.
- coherence: Every different valid typing derivation for a program leads to a resulting program that has the same dynamic semantics.

- There are actually a lot of similarity between adhoc polymorphism and parametric polymorphism.
- If a polymorphic function can depend on it's argument's type, it's adhoc polymorphism, otherwise it's parametric polymorphism.


Reason:

###### Function overloading

C++ style function overloading

```c++
void show(double d) {
  std::cout << "double value is: " << d << std::endl;
}

void show(char c) {
  std::cout << "oh hoho" << c << std::endl;
  std::cout << "char value is: " << c << std::endl;
}

void show(double d, double b) {
  std::cout << "doubles are: " << d << " " << b << std::endl;
}
```

Typescript style function overloading

- not real overloading
- it's really just optional parameter, promotes to the type level.
- You can have different signatures for present and absent of a type paramter.
```typescript
interface Show {
  show(d: number): string,
  show(): string
}

class Point implements Show {
  constructor(public x: number, public y: number) {}

  show(d?: number) {
    if (d && typeof d === "number") {
      return `Point ${d}`;
    } else {
      rturn "Point";
    }
  }
}

let p = new Point(1, 2);
console.log(p.show(1));
console.log(p.show());
```

Relationship with optional argument.

###### Typeclass in Haskell
- The point of typeclass
- relise on instance to be canonical (at ost one typeclass for each type)
- this is why orphan instance can cause a problem. (which instance to use?)
- implementation: (type directed implicit parameter passing mechanism)
- typeclass constraint can be inferred from function definition.

```haskell
data DoubleType = Double Double

instance Show DoubleType where
  show (Double d) = show d
```

- multi parameter type class
```haskell
```

- think typeclass as type level predicate
```haskell
```

###### Traits in Rust


Rust style Traits

###### Interfaces

Java  style interface

###### Simulate operator overloading in common lisp

```lisp
(defpackage #:a-package
  (:use "COMMON-LISP")
  (:shadow ">")
  (:shadow "<"))

(defclass node ()
  ((name
     :type symbol :initarg :name
     :accessor name :initform nil)
   (distance
     :type number :initarg :distance
     :accessor distance :initform most-positive-fixnum)
   (predecessor
     :type node :initarg :predecessor
     :accessor predecessor :initform nil)))

(defmacro operator-overload (op &rest definitons)
  (let ((fname (read-from-string (concatenate 'string "binary" (symbol-name op)))))
    `(values
       (defun ,op (&rest ts)
         (reduce  (quote ,fname) (cdr ts) :initial-value (car ts)))
       (defgeneric ,fname (a b)
         ,@(loop :for def :in definitons :collect
                 `(,@def))))))

(operator-overload >
                   (:method ((a number) (b number)) (cl:> a b))
                   (:method ((a node) (b node)) (cl:> (distance a) (distance b))))

(operator-overload <
                   (:method ((a number) (b number)) (cl:< a b))
                   (:method ((a node) (b node)) (cl:< (distance a) (distance b))))

```

### Row polymorphsm

- 1. {l₁ = e₁, l₂ = e₂ ..., ρ}
- 2. ρ for polymorphic rows
- 3. three operations
  - select
  - add
  - remove


```ocaml
```

```typescript
type Type = {
  a: string,
  b: number,
  c: string
}

function foo(e: {a: string}) {
  console.log(e.a);
}
```
