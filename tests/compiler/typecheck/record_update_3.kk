let a = { x = 2, y = { z = 3} }
let b = a.{ x => \_ => "hi" } // expected Integer, got String