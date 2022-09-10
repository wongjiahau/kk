let foo = (x: String): [String] => [x]

{
  let a = { x: 2, y: {z: 3} }
  let b = a.{ x.foo() } // expected String, got Int
  null
}