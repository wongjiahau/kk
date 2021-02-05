let foo = \(x, y) => [x, y]

let match = \(x, f) => x.f()

do "hello".match(.foo("world")).print()