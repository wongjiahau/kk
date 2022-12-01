let foo (s: String): String = "Hello"

let foo (i: Int): String = "World"

entry
  () .foo .print
//    ^^^ Error: no matching overloaded function found
