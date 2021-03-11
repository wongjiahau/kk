let foo 
  : | String => String
  = | _ => "Hello"

let foo
  : | Integer => String
  = | _ => "World"

do true.foo.print