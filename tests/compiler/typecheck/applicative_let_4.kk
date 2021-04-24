let map
  : | Integer | Integer => Integer => [Integer]
  = | x f => [x.f]

do 
  let/map 0 = 123
     // Error, non-exhaustive pattern
  null