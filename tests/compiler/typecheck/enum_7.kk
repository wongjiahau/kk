enum Food = Nut()
enum Tool = Nut()

let a: Tool = Nut()
let f = | Nut():Food => 123

do a.f()
