let foo<A> 
  : | A => Integer
  = | _ => 123

let z: Integer = 3
let x: {
  x: Integer
  y: String
  z: Integer
} = {
  x: null.foo() 
  y: "456" 
  z
}

do x.print()