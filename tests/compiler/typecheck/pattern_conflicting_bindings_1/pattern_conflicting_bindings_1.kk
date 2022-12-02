class Foo = A {a: Int, b: Int} | B

entry
  B .(
    
    \ A {a = x, b = x} -> ()
       //           ^ Error: duplicated binding `x`
  )
