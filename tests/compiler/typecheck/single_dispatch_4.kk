let foo = (s: String): String => "Hello"

let foo = (i: Integer): String => "World"

do true.foo().print()
//      ^^^^^ Error: no matching overloaded function found