let foo (_: String): String = "Hello"

let foo (_: Int): String = "World"

// map1 takes function last
let<A, B> map1 (x: A) (f: A -> B): B = f (x)

// map2 takes function first
let<A, B> map2 (f: A -> B) (x: A) : B = f (x)

entry
  123 .map1 (foo) .print; // "World"
  123 .map1 (foo) .map { x -> x foo } .print; // "Hello"

  123 .map2 (foo) .print; // "World"
  foo .map2 (123) .map2 { x -> x foo } .print // "Hello"
