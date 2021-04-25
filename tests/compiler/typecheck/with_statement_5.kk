let apply = (x: Integer, f: (a: Integer, b: Integer) => Integer): Integer => x.f(x)

do {
  with x = 123.apply;
    // ^ Error: expected 2 arguments but got only 1
  null
}