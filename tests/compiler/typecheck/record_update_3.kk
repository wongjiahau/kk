let a = { x: 2 }
let foo = (_: Integer): String => "Hello"
do {
  let b = a.{ x.foo() } // expected Integer, got String
}