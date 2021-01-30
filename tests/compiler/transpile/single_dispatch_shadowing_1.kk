let foo = \(_: String) => "Hello"

do 
  let foo = \(_: String) => "World"
  "x".foo().print()

do "x".foo().print()