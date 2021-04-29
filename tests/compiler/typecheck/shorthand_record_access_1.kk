let match = <A, B>(a: A, f: (a: A) => B): B => a.f()

{ a: 2 }.match(.b).print()