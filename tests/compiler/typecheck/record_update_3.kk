let a = { x 2 }
let foo = | _:Integer => "Hello"
let b = a.{ x.foo() } // expected Integer, got String