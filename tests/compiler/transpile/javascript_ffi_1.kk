let minus
  : | Integer Integer => Integer 
  = @@@ (x, y) => x - y @@@

let multiply
  : | Integer Integer => Integer 
  = @@@ (x, y) => x * y @@@

let less_than
  : | Integer Integer => Boolean
  = @@@ (x, y) => x < y @@@

let match<A B>
  : | A | A => B => B
  = | x f => x.f()
    
let factorial
  : | Integer => Integer
  = | n => n.less_than(2).match(
      | true => 1
      | false => n.multiply(n.minus(1).factorial())
    )

do 21.factorial().print()