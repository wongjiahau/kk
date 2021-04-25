let foo: {
  f:  (options: { x: Integer }) => Integer
} = {
  f: options => options.y
}