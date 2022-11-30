
type Foo {
  A({a: Foo, b: Foo}),
  B
}

switch(B) {
  \ A({a: A(x), b: x}): true
        //            ^ Error: duplicated binding `x`
}