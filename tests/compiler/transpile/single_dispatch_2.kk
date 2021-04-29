let foo = (_: String): String => "Hello"

let foo = (_: Boolean): String => "World"

let map = <A, B>(x: A, f: (a: A) => B): B => x.f()

true.map(foo).print()
true.map(foo).map(x => x.foo()).print()
