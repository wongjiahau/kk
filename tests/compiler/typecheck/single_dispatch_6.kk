let (x : Int) foo (y : Int) : Int = 123

let (x : Float) foo (y : Float) : Float = 789.0

entry
  9 foo 8 print;
  9.0 foo 8.0 print;
  1.0 foo 2 print // compile error
