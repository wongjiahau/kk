let foo: {
  f:  (options: { x: Int }) => Int
} = {
  f: options => options.y
}