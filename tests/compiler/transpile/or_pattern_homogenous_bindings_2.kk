enum Foo {
  A(String),
  B({hello: String})
}

let toString = (foo: Foo): String =>
  switch foo {
    case A(x) | B({hello: x}): x
  }


A("Spongebob").toString().print()
B({hello: "Squarepants"}).toString().print()
