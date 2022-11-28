let foo (_: String): String = "Hello"

let foo (): String = "World"

entry
  "test" foo.print;
  foo ().print;
  foo () foo.print
