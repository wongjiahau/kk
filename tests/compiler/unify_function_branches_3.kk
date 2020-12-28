enum Boolean = #true #false
let result = \x => x.(
  \#true => 1
  \#false => 'yo'
)()
