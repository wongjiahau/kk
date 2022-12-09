class Foo 
  = A { x: String, y: String }
  | B { a: String, b: String }

let toString
  : Foo -> String
  = 
    // More tha one binding per pattern
    -> A { x, y } | B { a = x, b = y } ["${x} - ${y}"]

entry
  A { x = "Spongebob", y = "Squarepants" } .toString .print;
  B { a = "Squiward", b = "Patrick" } .toString .print
