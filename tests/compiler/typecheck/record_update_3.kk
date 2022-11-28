let foo (_: Int): String = "Hello"

entry
  let a = (x = 2);
  let b = a.(x = a.x foo); // expected Int, got String
  "Hello" .print
