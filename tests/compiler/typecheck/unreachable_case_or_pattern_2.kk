type Foo {
  A,
  B
}

let foo = (x: Foo, y: Foo): Boolean -> 
  switch {x, y} {
    case {x: A | B, y: A}: true
    case {x: A | B, y: A | B}: true
      // ^^^^^^^^^^^^^^^^^^^^ 
      // Error: this pattern contains a an unreachable subpattern, namely:
      //    {x: A, y: A}
  }