enum Foo {
  A(String),
  B(Boolean)
}

let get = (foo: Foo): Boolean =>
  switch(foo) {
    case A(x) | B(x): true
            //    ^ Error: expected `String` got `Boolean`
  }