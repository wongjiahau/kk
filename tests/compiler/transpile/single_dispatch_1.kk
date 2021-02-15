let foo = | _: String => "Hello"
let foo = | _: Boolean => "World"

do "test".foo().print()
do true.foo().print()
do true.foo().foo().print()