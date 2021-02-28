type R = { x: Integer y: { z: Integer }}
let a: R = { x: 2 y: { z: 3 } }
let b: R = a.{ y.{ z: 4 }}

do a.print()
do b.print()