let and = (x: Boolean, y: Boolean): Boolean ->
  switch ({ x, y }) {
    \ { x: true, y: true }: true
    \ _: false
    \ _: false
  }