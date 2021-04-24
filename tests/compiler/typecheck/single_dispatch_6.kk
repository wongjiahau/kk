let foo = (x: Integer, y: Integer): Integer => 123

do 9.foo(8).print()

let foo = (x: Float, y: Float): Float => 789.0

do 9.0.foo(8.0).print()

do 1.0.foo(2) // compile error