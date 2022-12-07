# Overloading

Every top-level name (not just function!) can be overloaded as long as their
type does not overlaps.

The overlapping rule is simple, a type variable overlaps with any type, and any
other type overlaps with itself.

Examples of non-overlapping pairs:

```
Int -> String
Int -> Int -> String
```

```
Int
{ x: Int, y: String }
```

Examples of overlapping pairs:

```
<A> A -> String
Int -> String
```

## Overloading Resolution

The overloading resolution algorithm is not complete (because a complete
algorithm will be slow, and also produces monstrous error messages), thus type annotations
are sometimes required.

For example:

```kk
let x: Int = 2
let x: String = "Hello"

let foo (a: String): () = a .print

entry
  x .foo; // This is ok, the type checker knows `x` is type of `String` because of `foo`

  let y = x; // This is not ok, as there's no way to know which `x` is being referred

  let z: Int = x; // This is ok

  ()
```

## Shadowing

Like Rust, a variable can be shadowed using the same name:

```kk
let main (): () =
  let x = 1;
  x .print; // 1
  let x = "Yo";
  x .print // "Yo"
```
