let foo 
  : | String => String
  = | _ => "Hello"

let foo 
  : | Boolean => String
  = | _ => "World"

do "test".foo.print
do true.foo.print
do true.foo.foo.print