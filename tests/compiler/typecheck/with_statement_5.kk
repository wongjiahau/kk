let apply = (x: Int, f: (a: Int, b: Int) -> Int): Int -> x.f(x)

{
  with x = 123.apply
    // ^ Error: expected 2 arguments but got only 1
  null
}