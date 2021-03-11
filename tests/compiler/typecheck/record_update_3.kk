do
  let a = { x: 2 }
  let foo = | _ => "Hello"
  let b = a.{ x.foo } # expected Integer, got String
  null