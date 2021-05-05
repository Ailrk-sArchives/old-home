-- tag C++ template
-- title C++ Template as a Langauge
-- time 2021-04-04
-- source https://docs.microsoft.com/en-us/cpp/cpp/templates-cpp?view=msvc-160
          https://tismith.id.au/2014/template-metaprogramming-fun.html
          http://matt.might.net/articles/c++-template-meta-programming-with-lambda-calculus/
          https://rtraba.files.wordpress.com/2015/05/cppturing.pdf
          http://www.tnkcs.inf.elte.hu/vedes/sinkovics_abel_ertekezes.pdf
          https://dl.acm.org/doi/pdf/10.1145/234286.1057836
          https://github.com/ailrk/tml
          http://www.erwin-unruh.de/primorig.html
;;
## C++ Template as A Langauge

<br/>

Templates was added to C++ in it's third release (1991) to support parametric polymorphism, and it was soon be discovered to be much more powerful then it's original design goal. It was proven to be [turning complete](https://rtraba.files.wordpress.com/2015/05/cppturing.pdf), although you cannot directly perform IO with it, technically you can do any other computations with just templates and partial specializations. The earliest recorded template abuse I could find was [this little program](http://www.erwin-unruh.de/primorig.html) written by Erwin Unruh in 1994 to calculate prime at compile time. Ever since then, C++ templates have became a powerful tool not only enpowering generic programming and meta programming techniques, but also being an ideal tool for recreational programming purpose :p.

I find the description "turning complete" a bit unsatisfied. I mean sure you can encode some turing machines and watch the compiler move tapes, or you can implement a [sicp style interpreter](http://matt.might.net/articles/c++-template-meta-programming-with-lambda-calculus/), which itself is turning complete. But I want to use C++ templates to do some real programming. I was imagining a full lambda calculus based functional programming langauge augmented with useful libraries, maybe in monadic style or cps style to make it actually usable for relatively big projects. Apparently this idea has already been explored 7 years ago in [this](http://www.tnkcs.inf.elte.hu/vedes/sinkovics_abel_ertekezes.pdf) dissertation. The paper has a comprehensive discussion about how to do functional style programming with C++ templates. Following it's lead, I implemented my own version. Here is a little snippet to demonstrate the effect:

```c++
#include "tml.h"
#include <iostream>
declare(x); declare(y); declare(z);

template <typename N>
struct fact
    : if_<apply<less, N, int_<1>>,
          int_<1>,
          apply<times,
                fact<
                    apply<minus,
                          N,
                          int_<1>>>,
                N>
          > {};

using func =
    lambda<x, y>::begin<
        let<z, int_<2>,
            in<fact<
                apply<plus,
                      apply<plus, x, y>,
                      apply<times, z, z>
                      >>>>>;

int main(void) {
    std::cout << "print it: "
              << apply<func, int_<3>, int_<4>>::value
              << std::endl;
    return 0;
}

// $ print it: 3628800
```
This example shows some basic features like let binding, lambda, and currying. Later we will add data types with monadic interfaces like maybe type and list type. When I was implementing it I found a lot of topics for implementing a programming language applies here as well. For example, you need to specify what is considered as a value, what comprises an expression, and what does it mean to evaluate something, all in a C++ template setting.


### Mechanisms
Before we jump into the details, let's refresh mechanisms that makes C++ templates turing complete.

###### Templates
First we know template is completely a compile time concept, they parameterize a body of definiion with a template variable, much like how functions parameterize a body of code. The difference is function takes values as parameters, but templates take types. We are particularly interested in class temlate, because one can think a class template as a mapping from a type to another type, which resembles the behavior of a function at term level:
```c++
template <typename A>
struct add_const_reference{
  using type = const A&;
};
```
If we substitute A with `int`, the example above will generate a full specialization of type `add_const_reference<A>` when it's applied with a concrete type. To get the output of this specialization, we access it's `type` member alias as `add_const_reference<int>::type`. One thing to note is, if there is recursion going on, a full specilization without accessing the `::type` will not further specialize the recursive step (aka right hand side of the type alias), which makes it behaves much like lazy evaluation. This is important. Because we can choose to defer the specialization, it's easy to implement control flow syntax like `if` an short circuit operators like `or` without assistence outside of the language.

###### Partial specialization
Another empowering feature is template partial specilization. Partial specialization works like pattern matching in many functional programming languages, it invokes a specific specialization when a certin types are passed as arguments. Take an example:

```c++
template <typename T, typename U>
struct Klass {
  using type = int;
};

template <typename T>
struct Klass<T, T> {
  using type = double;
};

template <typename T, typename U>
struct Klass<T, U*> {
  using type = char;
};
```
We defined the same template `Klass` and three different specialization when it it's applied with different type paramters. If we have `Klass<int, double *>`, the template deduction rule will deduce `T` as `int` and `U` as `double`, and the second template parameter is in the form `U*`, which match the third specialization perfectly. So if we access `Klass<int, double*>::type`, it will give us a `char` type.


###### Varadic template

Template allows us to parameterize over type definiton, partial specialization allows us to select what to specialize based on different cases. These two features along are alreay enough to make a full lambda calculus. To make things better, we now have varadic template, which allows us to pattern match on arbitrary long type list. For example, if we have a type level list, we can mimic a `head` function easily with varadic template:

```c++
struct nil {};
template <typename...> struct head ;
template <typename T, typename... Ts> struct head<T, Ts...>  {
  using type = T;
};
template <> struct head<> { using type = nil; };
```

###### Template template paramter

As we already known, templates can be thought as type level function that takes a type and yields a new type. When we are doing partial specialization, how do we match on a template that suppose to work like a function? For that C++ template provides `template template parameter`, or it's better known as higher kinded type.


```c++
```

### Metafunction

### Values

### Evaluation Strategy

### Let Binding

### Calling conventions.

### Lambda!

### Typeclass default implementations

### Monads

### Make it a library

### Conclusion

// TODO unfinished 2021-04-07
