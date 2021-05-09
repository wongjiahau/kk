enum Foo {
  A({x: String, y: String}),
  B({a: String, b: String})
}

let toString = (foo: Foo): String =>
  switch foo {
    // More tha one binding per pattern
    case A({x, y}) | B({a: x, b: y}): "${x} - ${y}"
  }


A({x: "Spongebob", y: "Squarepants"}).toString().print()
B({a: "Squiward", b: "Patrick"}).toString().print()