type Foo = 
  case a (String) 
  case b #{hello: String}

let [to string]
  : Foo -> String
  = { case a(x) | b #{hello = x} -> x }

entry
  a ("Spongebob") [to string] print;
  b #{hello = "Squarepants"} [to string] print
