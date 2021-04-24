let apply
  : | Integer | Integer Integer => Integer => Integer
  = | x f => x.f(x)

do 
  let/apply x = 123
     // Error, non-exhaustive pattern
  null