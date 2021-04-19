function foo(_: String): String
  "Hello"

function foo(_: Boolean): String
  "World"

function map<A, B>(x: A, f: (a: A) => B): B
  x.f()

do true.map(foo).print()
do true.map(foo).map(x => x.foo()).print()
