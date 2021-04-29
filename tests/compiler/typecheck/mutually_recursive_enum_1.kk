enum Foo {
  Foo(Bar),
  NullFoo
}

enum Bar {
  Bar(Foo),
  NullBar
}

Foo(Bar(Foo(NullFoo))).print() // Compile Error