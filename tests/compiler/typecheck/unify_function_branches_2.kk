enum Boolean = True() False()
enum Bomb = Bomb()
let and: \(Boolean, Boolean) => Boolean = 
  \(True(), Bomb()) => True
