
type Foo = #a(x: String, y: String) | #bala

entry 
  {
    \#a (x, y)  | #bala -> true
            //    ^^^^^ Error: missing binding `x`
  } (#bala)
