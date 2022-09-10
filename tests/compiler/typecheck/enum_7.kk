enum Food { Nut }
enum Tool { Nut }

let a: Tool = Nut
let f = (Nut: Food): Int => 123

a.f()
