let identity<A> 
  : | A => A
  = | x => x

do
  let x = "hello".identity()
  let y: Integer = x
  null
