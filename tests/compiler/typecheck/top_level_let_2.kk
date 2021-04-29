// Top level destructuring
let {x, y} = {
  x: (i: Integer): String => "Hello",
  y: (b: Boolean): String => "World"
}

1.x().print()
true.y().print()