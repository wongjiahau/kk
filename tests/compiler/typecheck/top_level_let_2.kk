// Top level destructuring
let {x, y} = {
  x: (i: Integer): String => "Hello",
  y: (b: Boolean): String => "World"
}

do 1.x().print()
do true.y().print()