let map = (x: Integer, f: (n: Integer) => Integer): [Integer] => [x.f()]

do {
  with 0 = 123.map;
     // Error, non-exhaustive pattern
  null
}