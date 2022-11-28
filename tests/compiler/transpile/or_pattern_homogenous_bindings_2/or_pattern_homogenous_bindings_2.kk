class Foo = a (String) | b {hello: String}

let `to string`
  : Foo -> String
  = \a(x) | b {hello = x} -> x

entry
  a "Spongebob". `to string` .print;
  b {hello = "Squarepants"} .`to string` .print
