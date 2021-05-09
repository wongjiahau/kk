
enum Foo {
  A({a: Foo, b: Foo}),
  B
}

switch(B) {
  case A({a: A(x), b: x}): true
        //            ^ Error: duplicated binding `x`
}