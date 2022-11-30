type Foo = #a | #b

let foo (x: Foo) (y: Foo): Int =
  {
    \(x = #a | #b, y = #a) -> 1
    \(x = #a | #b, y = #a | #b) -> 1
  //  ^^^^^^^^^^^^^^^^^^^^^^^^
  // Error: this pattern contains a an unreachable subpattern, namely:
  //    {x: A, y: A}
  } (x, y)
