enum Boolean = #true #false
enum Bomb = #bomb
let and: \(Boolean, Boolean) => Boolean = 
  \(#bomb, #true) => #true
