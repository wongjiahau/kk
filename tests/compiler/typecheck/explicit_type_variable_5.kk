let arrayMapFirst<From, To> = \(
  xs: Array<Element= From>, 
  f: \From => To
): Array<Element= To> =>
  let [head, ..._] = xs else \_ => []
  [head.f()]

let result = [{x= 3}].arrayMapFirst<To= string>(\_ => 2)
