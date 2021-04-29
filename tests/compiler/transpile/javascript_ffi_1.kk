let minus = (x: Integer, y: Integer): Integer => @@@ _0 - _1 @@@

let multiply = (x: Integer, y: Integer): Integer => @@@ _0 * _1 @@@

let less_than = (x: Integer, y: Integer): Boolean => @@@ _0 < _1 @@@

let factorial = (n: Integer): Integer =>
  if(n.less_than(2)) 
    1
  else  
    n.multiply(n.minus(1).factorial())
  
21.factorial().print()