let foo
  : | String => String
  = | String => "Hello"

do 
  let foo = | _ => "World"
  "x".foo.print

do "x".foo.print