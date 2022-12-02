class Boolean = true | false
class Foo = A(String) | B(Boolean)

let get (foo: Foo): Boolean =
  foo .( 
    \ A(x) | B(x) -> true
        //     ^ Error: expected `String` got `Boolean`
 )
