let foo 
  : | String => String
  = | _ => "Hello"

let foo 
  : | Boolean => String
  = | _ => "World"

let bar: Integer = 
  let f = | x => x.foo()
  123