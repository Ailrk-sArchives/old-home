-- tag rust smark-pointer memory-management
-- source rustbook
-- title A potential article about rust smart pointer
-- date 2020-07-03
;;
## A potential article about rust smart pointer.

 A pointer is a general concept for a variable that contains an address __in memory__. This address refers to, or "point at", some other data. The most common   kind of pointer in Rust is a reference, which you learned about in `chapter 4`.   References are ndicated by the & symbol and borrow the value they point to.   They don't have any special capabilities other than referring to data. Also,   they don't have any overhead and are the kind of pointer we use most often.
  We've already encountered a few smart pointers in this book, such as String a  nd Vec<T> in Chapter 8. alghough we didn't call them smart pinters at the tim  e. Both these types count as smart pointers because they own some memory and   allow you to manipulate it. THey also have metadata (such as their capacity)   and extra capabilitiesor guarantees (uch as with String ensuring its data wil  l always be valid UTF-8).
  data they point to.
i
  Smart pointers, on the other hand, are data structures that not only act like   a pointer but also have aditional metadata and capabilities. The concept of   smart pointers isn't unique to Rust: smart pointers originated in C++ and exi  st in other languages as well. In Rust, the different smart pointers defined   in the standard library provide unctionality beyond that provided by referenc  es. One example that we'll explore in this chapter is the reference counting   smart pointer type. This pointer enables you to have multiple owners of data   by keeping track of the number of owners and, when no owners remain, cleaning   up the data.

```rust
fn good() -> uint8 {
    2;
}
```
  In Rust, which uses the concept of ownership and borrwowing, and additional d  ifference between references and smart pointers is that references are pointe  rs that only borrow data; In contrast, in many cases, smart pointers own the   data they point to.

```
namespace cvalgo {
template <typename T, typename U>
auto conv(Mat<T> &mat, Mat<U> &kern) -> Mat<T> { // convolution is assositive
static_assert(kern.cls == kern.rows, "filter needs to be square matrix");
auto result = Mat<T>::zeros(mat.size());
auto width = kern.cols;

for (size_t i = 0; i < mat.rows; ++i) {
  for (size_t j = 0; j < mat.cols; ++j) {
    // if at boundary.
    // TODO
    // normal case
  }
}
return result;
}
```
  We've already encountered a few smart pointers in this book, such as String a  nd Vec<T> in Chapter 8. alghough we didn't call them smart pinters at the tim  e. Both these types count as smart pointers because they own some memory and   allow you to manipulate it. THey also have metadata (such as their capacity)   and extra capabilitiesor guarantees (uch as with String ensuring its data wil  l always be valid UTF-8).
  data they point to.
  \(a^2 + b^2 = c^2\)
  \[ e = sum_(n=0)^oo 1/n! \]
 $$\forall x \in X, \quad \exists y \leq \epsilon$$
i
  We've already encountered a few smart pointers in this book, such as String a  nd Vec<T> in Chapter 8. alghough we didn't call them smart pinters at the tim  e. Both these types count as smart pointers because they own some memory and   allow you to manipulate it. THey also have metadata (such as their capacity)   and extra capabilitiesor guarantees (uch as with String ensuring its data wil  l always be valid UTF-8).
  data they point to.
  p
i
