let foo = \(_: String) => "Hello"
let foo = \(_: Boolean) => "World"

let map = \(x, f) => x.f()

do true.map(foo).print()
do true.map(foo).map(\x => x.foo()).print()
