type Foo = 
  case a #{ x: String, y: String }
  case b #{ a: String, b: String }

let toString
  : Foo -> String
  = {
    // More tha one binding per pattern
    case a #{ x, y } | b #{ a = x, b = y } -> "${x} - ${y}"
  }

entry
  a #{ x = "Spongebob", y = "Squarepants" } toString print;
  b #{ a = "Squiward", b = "Patrick" } toString print
