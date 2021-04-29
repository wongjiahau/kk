let foo = <T>(x: T, y: T): [T] => [x, y]

"hello".(.foo("world")).print()