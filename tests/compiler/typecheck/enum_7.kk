type Food { Nut }
type Tool { Nut }

let a: Tool = Nut
let f = (Nut: Food): Int -> 123

a.f()
