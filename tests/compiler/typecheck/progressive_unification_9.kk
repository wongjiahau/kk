let bar = \(props: {
  f: \{ x: Number } -> Number
}) =>
  let { f } = props
  {x = 'hi'}.f()

