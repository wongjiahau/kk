let foo = <T>(x: T, y: T): [T] => [x, y]

do "hello".(.foo("world")).print()