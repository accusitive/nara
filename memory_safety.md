
# Region Inference and Type Annotation Notes

### General syntax notes
Functions area inherently polymorphic over region, as in:
```rust
fn add_two(val: i32) -> i32 {}
```
where does `val` reside? in it's own region, imagine the function as
```rust
fn add_two<'a, 'b>(val: i32 @ 'a) -> i32 @ 'b
```
effectively, if a parameter does not specfiy it's region, a new parametric region variable is created and added to the type signature

NOTE: we DO NOT have syntax for specifying regions this way. its only a documentation construct to make things clearer

## 1. Function: `allocate_at`

```rust
fn allocate_at<T>(location: (), value: T) -> &T {
//                          ^ '0       ^ '1   ^ '2

    let p: &T = value at location;
//         ^ '3 ==       ^ '0   
    p
    // '2 == '3
}
```
### Region Naming:

* `'a`: Region of `value` (i.e. `'1`)
* `'r`: Region of `location`, `&T`, and return (i.e. `'0 == '2 == '3`)

### Signature (fully elaborated):

```rust
allocate_at<'r, 'a, T>(location: () @ 'r, value: T @ 'a) -> &T @ 'r
```

---

## 2. Function: `some_math`

```rust
fn some_math(location: (), base: i32 @ location, another: i32) -> i32 @ location {
    (base + another) at location
}
```

* The result stays in the same region as `base` and `location`.

---

## 3. Function: `map`

```rust
fn map<T, U>(value: T, mapper: fn(T @ value) -> U @ value) -> U @ value {
//                  ^'1        ^'2^'0   ^'1           ^'1 

    mapper(value) // -> U @ value
}
```

### Implicit Region Elaboration:

```rust
fn map<'a, 'b, T, U>(
    value: T @ 'a,
    mapper: (fn(T @ 'a) -> U @ 'a) @ 'b
) -> U @ 'a
```

### Example Usage:

```rust
fn showcase() { // '0
    let value = new 42; // allocated in '0
    let wrapper = |x| { Option::Some(x) } // maybe 'global?
    let m = map(42, wrapper);
}
```

### Lifted Version of Closure:

```rust
fn wrapper_lifted<T>(x: T) -> Option<T> {
//                      ^'0          ^'0
    Option::Some(x)
}
```

---
# Some ADT Examples
### Double

```rust
type Double<T> = {
    first: T,
    second: T
}
```

Can be considered as (do note, we *do not* have this <'a> life time syntax, its just a very clear way to represent region polymorphism):

```rust
type Double<'a, 'b, T> = {
    first: T @ 'a,
    second: T @ 'b
}
```

---

### OneAfterAnother

```rust
type OneAfterAnother<T> = {
    first: T,
    second: T @ first
}
```
Can be considered as
```rust
type OneAfterAnother<'a, T> = {
    first: T @ 'a,
    second: T @ 'a
}
```
---

## 5. Function: `test` (inference showcase)

```rust
fn test(arena: ()) -> Double<i32> {
    // theoretically, region can be inferred?

    // lets call arena's region 'a
    // region looks like: Double {first: 'a, second: 'a} therefore Double is Double<'a, 'a, i32>, therefore Double {first: i32 @ 'a, second: i32 @ 'a}
    Dobule { 
        first: 42 at arena 
        second: 42 at arena
    }
}
```

---

## 6. Type: `Something` and Region Unification

```rust
type Something = {
    value: i32
}
```

```rust
fn ret_something(val: i32) -> Something {
//                    ^'0                             ^'1

    Something {
        value: val // unify '1 of Something { value: '1 } = '0
    }
}
```