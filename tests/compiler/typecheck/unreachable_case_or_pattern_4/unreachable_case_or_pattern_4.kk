type Foo = #A(Foo) | #B

let spam
  : Foo -> String 
  = {
    \#A(x) | #A(#A(x) | #A(#A(x))) -> "test"
    \ _ -> "Nope"
  }


