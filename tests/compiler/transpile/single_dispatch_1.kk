let foo = (_: String): String => "Hello"

let foo = (_: Boolean): String => "World"

do "test".foo().print()
do true.foo().print()
do true.foo().foo().print()