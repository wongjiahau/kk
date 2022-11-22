let foo (s: String): String = "Hello"

entry
  "x" foo print; // "Hello"
  let foo : String -> String = { x -> "World"};
  "x" foo print // "World"
