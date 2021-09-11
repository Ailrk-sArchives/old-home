-- tag C++
-- title Destructive Move
-- time 2021-08-02
-- source https://en.cppreference.com/w/cpp/language/move_constructor
          https://www.foonathan.net/2017/09/destructive-move/#content
          https://radekvit.medium.com/move-semantics-in-c-and-rust-the-case-for-destructive-moves-d816891c354b
          https://stackoverflow.com/questions/32300132/why-cant-i-store-a-value-and-a-reference-to-that-value-in-the-same-struct
          http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2002/n1377.htm
          inside_C++_object_model
;;

## Destructive move

Move semantics is simple! It's derived from idioms that people adopt to avoid expensive heap allocations. Actually, it's just `memcpy`: If I have an object on the stack that holds a pointer to a large piece of heap memory, and I want to give it away to someone else (e.g another object, or a function), there are two things I need to do: first, I copy the object on the stack byte by byte to the new location; second, I invalidate the old object so nobody can touch it.

This simple model is called `destructive move`, which is, very unfortunately, not how C++ move semantics work. The existence of exceptions and the ability to have self referential structs force us to have a much more complicated mechanism.

### What does it mean to move?
`Move` is a higher order concept. For CPU, there is no sense of moving. The only way to relocate a piece of memory is to copy: We can read some bytes, put it into the register, then write them into another address. When we talk about moving a byte from address A to address B, it's from the point of view that before we "move", the byte is at address A, and after we move it's at address B. The object is "moved" if you ignore the fact that copying was actually happening behind the scene and there is a moment that the same data exists at both addresses.

```c++
template <typename T, typename... Args> class Movable {
  bool not_moved = true;
  T content_;
  T (*clone_)(void *);
  Movable(Movable<T> &to, T *from)
    { std::memcpy(&to.content_, from, sizeof(T)); }

public:
  Movable(Args &&...args)
      : content_(T{std::forward<Args>(args)...}),
        clone_([](void *ptr) -> T { return *static_cast<T *>(ptr); }) {}

  T *operator->() {
    assert(not_moved && "access moved object");
    return &content_;
  }

  Movable clone() {
    assert(not_moved && "access moved object");
    return clone_(&content_);
  }

  Movable destructive_move(Movable<T> &to) {
    assert(not_moved && "access moved object");
    not_moved = false;
    return Movable(to, &content_);
  }
};
```

Try it

```C++
struct Foo {
  int *data, size;
  void report(const char *str) { /* ... */ }
  void fill_8() { /* allocate 8 bytes on heap, initalize to their index */ }

  ~Foo() { delete[] data; }
  Foo() : data(nullptr), size(0) {}
  Foo(const Foo &other) : size(other.size), data(new int[other.size]) {
    std::memcpy(data, other.data, sizeof(int) * other.size);
  }
  Foo &operator=(const Foo &other) { /* ...similar to the copy constructor */ }

  // assume we don't have move semantics.
};

void take_foo(Movable<Foo> &foo1) {
  Movable<Foo> foo2;
  foo1.destructive_move(foo2);      // move wfoo to foo1
  foo2->report("foo2 in take_foo"); // pointer still points to the same place
}

int main(void) {                             // 0
  Movable<Foo> foo;                          // 1 --+
  foo->fill_8();                             // 2   |
  foo->report("foo in main");                // 4 foo is destructively moved.
  take_foo(foo);                             // 3 --+
  foo->report("foo in main");                // 4 Assertion Fail!
  return 0;                                  // 6
}
```
Output

```
[reporting from foo in main] < 1 2 3 4 5 6 7 8 >
  | [calling destructive_move]
  | [Foo::~Foo()]
  | [reporting from foo2 in take_foo] < 1 2 3 4 5 6 7 8 >
  | [END of take_foo]
  | [Foo::~Foo()]
a.out: moveable.cc:23: T* Movable<T, Args>::operator->() [with T = Foo; Args = {}]: Assertion `not_moved && "access moved object"' failed.
fish: Job 1, './a.out' terminated by signal SIGABRT (Abort)
```

##### Exception safty

##### Self referential struct


### Move from state

### Historical remark

### How to handle move in C++

### Conclusion
