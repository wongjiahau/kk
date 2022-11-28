type Foo {
  A(String),
  B(Boolean)
}

let get = (foo: Foo): Boolean ->
  switch(foo) {
    \ A(x) | B(x): true
            //    ^ Error: expected `String` got `Boolean`
  }