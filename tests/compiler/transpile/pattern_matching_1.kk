let and = (x: Boolean, y: Boolean): Boolean =>
  switch ({ x, y }) {
    case {x: true, y: true}: true
    case _: false
  }


true.and(true).print()
true.and(false).print()
false.and(true).print()
false.and(false).print()
