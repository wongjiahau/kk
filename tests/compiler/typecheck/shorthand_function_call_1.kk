let foo = <T>(a: T, b: T): {a: T, b: T} => {a, b}

let match = <A, B>(a: A, f: (a: A) => B): B => a.f()

"hello".match(.foo(2)).print()