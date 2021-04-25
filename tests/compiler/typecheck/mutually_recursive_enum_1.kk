enum Foo {
  Foo(Bar),
  NullFoo
}

enum Bar {
  Bar(Foo),
  NullBar
}

do Foo(Bar(Foo(NullFoo))).print() // Compile Error