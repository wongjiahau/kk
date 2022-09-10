let foo (s: String): String = "Hello"

entry
  let foo : String -> String = { x -> "World"};
  "x" foo print

entry
  "x" foo print
