enum Option<T> = #none #some(T)

let firstIsTrue = \xs =>
  let [x, ..._] = xs
  let true = x
  true

