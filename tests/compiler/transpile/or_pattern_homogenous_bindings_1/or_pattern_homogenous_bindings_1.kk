enum Foo {
  A { x: String, y: String },
  B { a: String, b: String }
};

toString
  : (Foo -> String)
  = 
    "More than one binding per pattern"
    [
      [ A { x, y }, B { a = x, b = y } ] -> "${x} - ${y}"
    ];


A { x = "Spongebob", y = "Squarepants" } .toString .print;
B { a = "Squiward", b = "Patrick" } .toString .print
