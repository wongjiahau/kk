let match<A B> 
  : | A | A => B => B
  = | x f => x.f

do { a: 2 }.match(|.b).print