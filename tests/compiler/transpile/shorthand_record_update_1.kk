let match = <T, U>(t: T, f: (t: T) => U): U => t.f()

do { x: 2 }.match(.{ x: 3 }).print