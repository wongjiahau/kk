let map = (x: Int, f: (n: Int) => Int): [Int] => [x.f()]

{
  with 0 = 123.map
     // Error, non-exhaustive pattern
  null
}