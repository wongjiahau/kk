let match<A B> 
  : | A | A => B => B
  = | x f => x.f

do { x: 2 }.match(|.{ x: 3 }).print