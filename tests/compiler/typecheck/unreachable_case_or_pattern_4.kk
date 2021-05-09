enum Foo {
  A(Foo),
  B
}

let spam = (foo: Foo): String =>
  switch foo {
    case A(x) | A(A(x) | A(A(x))): "test"
    case _: "Nope"

  }


