type Foo = #a(String) | #b (hello: String)

let toString
  : Foo -> String
  = { #a(x) | #b (hello = x) -> x }

entry
  #a "Spongebob" toString print;
  #b (hello = "Squarepants") toString print
