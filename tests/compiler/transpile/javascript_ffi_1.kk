let minus
  : | Integer Integer => Integer 
  = @@@ (x, y) => x - y @@@

let multiply
  : | Integer Integer => Integer 
  = @@@ (x, y) => x * y @@@

let less_than
  : | Integer Integer => Boolean
  = @@@ (x, y) => x < y @@@

let factorial
  : | Integer => Integer
  = | n => n.less_than(2).(
      | true => 1
      | false => n.multiply(n.minus(1).factorial)
    )

do 21.factorial.print