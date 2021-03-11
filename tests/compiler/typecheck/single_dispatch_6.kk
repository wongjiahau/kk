let foo
  : | Integer Integer => Integer
  = | _ _ => 123

do 9.foo(8).print

let foo
  : | Float Float => Float
  = | _ _ => 789.0

do 9.0.foo(8.0).print

do 1.0.foo(2) # compile error