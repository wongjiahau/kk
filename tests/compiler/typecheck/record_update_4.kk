let foo = \(x: String) => [x]

let a = { x = 2, y = { z = 3} }
let b = a.{ x => foo } // expected Integer, got String