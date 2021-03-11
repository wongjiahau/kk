let foo<A>
  : | A A => [A]
  = | x y => [x y]

do "hello".(|.foo("world")).print