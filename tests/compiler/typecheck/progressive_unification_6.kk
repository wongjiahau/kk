let match = \(x, f) => x.f()

let arrayMap = 
  \([], _) => []
  \([head, ..._], f) => [head.f()]

let result = [{foo = 2, bar = 'hi'}].arrayMap(\o => o.zap)
