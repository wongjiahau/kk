type R =  (x : Int, y : (z : Int))

entry
  let a = (x = 2, y = (z = 3));
  let b = (a | y = (a.y | z = 4));
  a print;
  b print
