let match = | x f => x.f()

let array_map<From To> = |
  xs: [From]
  f: | From -> To
-> [To] =>
  xs.match(
    | [] => []
    | [head ..._] => [head.f()]
  )

let result: [String] = [{x: 2}].array_map(| o => o.x)
