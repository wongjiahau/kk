let foo: {
  f: \{ x: number } => number
} = {
  f = \o => o.y
}
