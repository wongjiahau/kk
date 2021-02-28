let foo<A>
  : | A => Integer
  = | _ => 123

let xs
  : [Integer] 
  = [
    null.foo() 
    {x: null.foo()}.x
  ]

do xs.print()