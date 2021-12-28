-- tag C++
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

[Templates](Templates) was added to C++ in it's third release (1991) to support parametric polymorphism, and it was soon be discovered to be much more powerful then it's original design goal. It was proven to be [turning complete](https://rtraba.files.wordpress.com/2015/05/cppturing.pdf), although you cannot directly perform IO with it, technically you can do any other computations with just templates and partial specializations. The earliest recorded template abuse was [this little program](http://www.erwin-unruh.de/primorig.html) written by Erwin Unruh in 1994 to calculate prime at compile time. Ever since then, C++ templates have became a powerful tool not only enpowering generic programming and meta programming techniques, but also being an ideal tool for recreational programming purpose :p.

I always feel just knowing template is "turning complete" is a bit unsatisfying. I mean sure you can encode some turing machines and watch the compiler move tapes, or you can implement a [sicp style interpreter](http://matt.might.net/articles/c++-template-meta-programming-with-lambda-calculus/), which itself is turning complete. But I want to use C++ templates to do some real programming. I was imagining a full lambda calculus based functional programming langauge augmented with useful libraries, maybe in monadic style or cps style to make it actually usable for relatively big projects. Apparently this idea has already been explored 7 years ago in [this](http://www.tnkcs.inf.elte.hu/vedes/sinkovics_abel_ertekezes.pdf) dissertation. The paper has a comprehensive discussion about how to do functional style programming with C++ templates. Following it's lead, I implemented my own version. Here is a little snippet to demonstrate the effect:

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
This example shows some basic features like let binding, lambda, and currying. Later we will add data types with monadic interfaces like maybe type and list type. When I was implementing it I found a lot of topics for implementing a programming language applies here as well. You need to specify what is considered as a value, what comprises an expression, and what does it mean to evaluate something, all in a C++ template setting.


### C++ Template Mechanisms
Before we jump into the details, let's first refresh on what we can do with C++ templates.

###### Templates
First of all, C++ template is completely a compile time concept. A template by itself is not a type or a function or anything else, it's just a receipe to tell the compiler how to generate certain code. To actually make the code appear, one need to explicitly instantiate the template with all template paramters known at the compile time. This mechanism is sometimes referred as monomorphization, and it's the strategy C++ uses to acheive parametric polymorphism with zero runtime overhead. For our concern, we only need to know that the compiler instantiation process can involve arbitrary computation, and we can have a lot of control over it.


###### Partial specialization
The most important feature for meta programming is `template partial specilization`. Partial specialization works like pattern matching in many functional programming languages, it invokes a specific specialization when certain types are passed as arguments. Take an example:

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

As we already known, templates can be thought as type level function that takes a type and yields a new type. To push the analogy further, we can have higher order functions at the type level by using `template template parameter`. This feature allows us to pass a class template as parameter of another template, just like how you can pass a function to another function.

```c++
#include<type_traits>
template <template <typename ...> typename F, typename ...Ts>
struct apply {
  using type = F<Ts...>::type;
};

using out_type = apply<head, int, double, char>::type

static_assert(std::is_same_v<our_type, int>);
```


### Meta function

Templates can be thought as type level functions, they parameterize a body of definion with a template variable, much like how functions parameterize a body of code. The difference is function takes values as parameters, but templates take types. We are particularly interested in class template, because one can think a class template as a mapping from a type to another type, which resembles the behavior of a function at term level. In C++ lingo, these templates are called `meta functions`, and they are the basic building blocks of template meta programming.

Take a look at the following example:

```c++
template <typename A>
struct add_const_reference{
  using type = const A&;
};
```

If we substitute A with `int`, the example above will generate a full specialization of type `add_const_reference<A>` when it's applied with a concrete type. To get the output of this specialization, we access it's `type` member alias as `add_const_reference<int>::type`.

It's a very good intution to treating templates as meta functoins that takes a type and return another type, but C++ templates was not designed for this use cases any way, so we have some problems to apply the

TODO

, but C++ template can take more than types as parameter, you can also pass any integral values. For example, take a look at the famous factorial example that every template meta programming tutorials demonstrate:

```c++
tempalte<int N>
struct fact {
  static constexpr value = N * fact<N - 1>::value;
}

tempalte<>
struct fact<0> {
  static constexpr value = 1;
};
```



### Values

### Evaluation Strategy
One thing to note is, if there is recursion going on, a full specilization without accessing the `::type` will not further specialize the recursive step (aka right hand side of the type alias), which makes it behaves much like lazy evaluation. This is important. Because we can choose to defer the specialization, it's easy to implement control flow syntax like `if` an short circuit operators like `or` without assistence outside of the language.


### Let Binding

### Calling conventions.

### Lambda!

### Typeclass default implementations

### Monads

### Make it a library

### Conclusion

// TODO unfinished 2021-04-07
