let foo = (_: String): String => "Hello"

let foo = (_: Boolean): String => "World"

"test".foo().print()
true.foo().print()
true.foo().foo().print()