let foo
  : | Integer => Integer
  = |.bar  # Error: Expected Integer, got String

let bar
  : | Integer => String
  = |.foo