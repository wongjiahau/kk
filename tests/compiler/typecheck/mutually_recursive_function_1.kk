let foo = (n: Integer): Integer => n.bar()

let bar = (n: Integer): String => n.foo()
//                                ^^^^^^^ Error: expected String, got Integer