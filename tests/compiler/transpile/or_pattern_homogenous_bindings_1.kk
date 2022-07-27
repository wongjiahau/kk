enum Foo 
  = #a {x: String, y: String}
  | #b {a: String, b: String}

let toString
  : Foo -> String
  = {
  // More tha one binding per pattern
  | #a {.x, .y} | #b {.a = x, .b = y} -> `${x} - ${y}`
  }

entry
  #a {.x = `Spongebob`, .y = `Squarepants`} toString print;
  #b {.a = `Squiward`, .b = `Patrick`} toString print
