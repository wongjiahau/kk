enum Boolean = #true #false
let and = \(x, y) => 
  let #true = x
  let #true = y
  #true

do #true.and(#true).print()
do #true.and(#false).print()
do #false.and(#true).print()
do #false.and(#false).print()
