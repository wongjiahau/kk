let bar = | props: {
  f: | { x: Integer } -> Integer
} =>
  let { f, } = props
  {x "hi"}.f()

