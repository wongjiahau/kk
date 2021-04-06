let map<From To> 
  : | [From] | From => To => [To]
  = | xs f => xs.(
      | [] => []
      | [head .._] => [head.f]
    )

let result: [String] = [{x: 2}].map(| o => o.x)
