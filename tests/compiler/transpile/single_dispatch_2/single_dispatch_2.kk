let foo (_: String): String = "Hello"

let foo (_: Int): String = "World"

let<A, B> (x: A) .map (f: A -> B): B = f (x)

entry
  123 .map (foo) .print;
  123 .map (foo) .map -> x [x .foo] .print
