let minus = (x: Int, y: Int): Int => @@@ _0 - _1 @@@

let multiply = (x: Int, y: Int): Int => @@@ _0 * _1 @@@

let less_than = (x: Int, y: Int): Boolean => @@@ _0 < _1 @@@

let factorial = (n: Int): Int =>
  if(n.less_than(2)) 
    1
  else  
    n.multiply(n.minus(1).factorial())
  
21.factorial().print()