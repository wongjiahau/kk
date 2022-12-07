# Bang notation

Bang notation is inspired for facilitating syntactical construct like
Javascript async/await, Rust `?` operator.

It is inspired by [Idris `!` notation](https://idris2.readthedocs.io/en/latest/tutorial/interfaces.html#notation).

## Usage

To use bang notation, one must first declare a boundary using `~`, then a
bind function (like Haskell `>>=`), lastly the expression to be translated.

In Javascript/Rust, to use `await`/`?`, you must only use it within a function,
however using bang-notation, there's no such restriction as long as the
boundary is defined properly.

For example, say we want to emulate Rust `?` operator.

```kk
class Option<T> = some (T) | none

let<T, U> (option: Option<T>) .unwrap (callback: T -> Option<U>): Option<U> =
  option .(
    \some (t) -> callback (t)
    \none -> none
  )

entry
  let x = ~unwrap (
    some {
      x =  some "hi" ! ,
      y = some 2 ! ,
      z =  ~unwrap (some { x = none ! , y = some 2 ! })
    }
  );
  x .print // some {x = 1, y = 2, z = none}
```

In this example, `unwrap` is the bind function.

## How it works?

It works by translating the tilde-bangs into [Continuation-passing
style](Continuation-passing style).

Every bang is lifted as a lambda variable.

For example:

```
~ bind (x! + (y!))
```

Is translated into:

```
x .bind (\temp1 -> y .bind (\temp2 -> temp1 + temp2))
```
