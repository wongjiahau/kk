let (x : Int) = 1;

let (x : String) = "hello";

let ((a : String).x : Int) = (a.print; 999);

(
  """
  "hello" 999
  """
  "hey".x.print;
  (y : String) = x;
  """
  hello
  """
  y.print;
  (z : Int) = x;
  """
  1
  """
  z.print
)
