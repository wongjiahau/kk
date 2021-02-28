let foo 
  : | Null => Integer
  = | _ => 123
let xs: [Integer] = 
  let x = null.foo()
  let y = {x: null.foo()}.x
  [x y]

do xs.print()