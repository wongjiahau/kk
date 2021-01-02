let bar = \(props: {
  f: \{ x: number } => number
}) =>
  let { f } = props
  {x = 'hi'}.f()

