// Top level destructuring
let {x, y} = {
  x: (i: Int): String => "Hello",
  y: (b: Boolean): String => "World"
}

1.x().print()
true.y().print()