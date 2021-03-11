enum Food = Nut
enum Tool = Nut

let a: Tool = Nut
let f
  : | Food => Integer 
  = | Nut => 123

do a.f
