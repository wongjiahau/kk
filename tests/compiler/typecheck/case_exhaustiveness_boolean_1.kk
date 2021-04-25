let and = (x: Boolean, y: Boolean): Boolean =>
  switch ({ x, y }) {
    case { x: true, y: true }: true
    case _: false
    case _: false
  }