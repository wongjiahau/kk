let x : Int = 123
let x : String = "Hello"
let (a : String) .x (b : Int) =
  a .print;
  b .print;
  456

entry
  x .x (x) .print // "Hello" 123 456
