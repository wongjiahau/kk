enum Boolean = #true #false
enum Bomb = #bomb
let and: \(Boolean, Boolean) => Boolean = 
  \(#true, #bomb) => #true
