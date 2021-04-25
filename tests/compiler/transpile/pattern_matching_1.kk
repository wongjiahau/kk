let and = (x: Boolean, y: Boolean): Boolean =>
  switch ({ x, y }) {
    case {x: true, y: true}: true
    case _: false
  }


do true.and(true).print()
do true.and(false).print()
do false.and(true).print()
do false.and(false).print()
