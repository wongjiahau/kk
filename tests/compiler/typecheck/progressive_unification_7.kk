let match = \(x, f) => x.f()

let arrayMap<From, To> = \(
  xs: Array<Element= From>,
  f: \From -> To
) -> Array<Element= To> =>
  xs.match(
    \[] => []
    \[head, ..._] => [head.f()]
  )

let result: Array<Element= String> = [{x = 2}].arrayMap(\o => o.x)
