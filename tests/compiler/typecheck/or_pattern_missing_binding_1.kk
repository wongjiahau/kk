
enum Foo {
  A({x: String, y: String}),
  Bala
}

switch(Bala) {
  case A({x, y}) | Bala: true
        //    ^^^^ Error: missing binding `x`
}