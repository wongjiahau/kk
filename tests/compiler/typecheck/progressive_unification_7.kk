let match = \(x, f) => x.f()

let arrayMap<From, To> = \(
  xs: [From],
  f: \From -> To
) -> [To] =>
  xs.match(
    \[] => []
    \[head, ..._] => [head.f()]
  )

let result: [String] = [{x = 2}].arrayMap(\o => o.x)
