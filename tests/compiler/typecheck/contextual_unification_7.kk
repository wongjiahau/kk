let match<A B>
  : | A | A => B => B
  = | x f => x.f()

let map<From To> 
  : | [From] | From => To => [To]
  = | xs f =>
  xs.match(
    | [] => []
    | [head ..._] => [head.f()]
  )

let result: [String] = [{x: 2}].map(| o => o.x)
