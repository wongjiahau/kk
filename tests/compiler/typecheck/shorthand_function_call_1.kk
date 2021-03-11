let foo<A> 
  : | A A => [A]
  = | x y => [x y]

let match<A B> 
  : | A | A => B => B
  = | x f => x.f

do "hello".match(|.foo(2)).print