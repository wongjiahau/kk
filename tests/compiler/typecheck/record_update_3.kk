let a = { x: 2 }
let foo = (_: Int): String => "Hello"
{
  let b = a.{ x.foo() } // expected Int, got String
}