let match<A B>
  : | A | A => B => B
  = | x f => x.f()

do { x: 2 }.match(|.x).print()