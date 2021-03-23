let power
  : | Integer Integer => Integer
  = @@@ Math.pow @@@

let apply
  : | Integer | Integer Integer => Integer => Integer
  = | x f => x.f(x)

do 
  let result = 
    let/apply x y = 3
    x.power(y)
  result.print