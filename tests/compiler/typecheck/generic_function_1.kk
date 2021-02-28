let identity<A>
  : | A => A
  = | x => x

let constant<A> 
  : | A => Integer
  = | _ => 2

let map<A B>
  : | A | A => B => B
  = | x f => x.f()

let x: String = "hello".map(identity)
let y: String = "hello".map(| _ => "yo")
let z: String = "hello".map(constant)
