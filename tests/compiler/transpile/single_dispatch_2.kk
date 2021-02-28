let foo 
  : | String => String
  = | _ => "Hello"

let foo
  : | Boolean => String
  = | _ => "World"

let map<A B>
  : | A | A => B => B
  = | x f => x.f()

do true.map(foo).print()
do true.map(foo).map(| x => x.foo()).print()
