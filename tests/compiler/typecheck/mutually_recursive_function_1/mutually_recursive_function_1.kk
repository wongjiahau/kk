let foo (n: Int): Int = n .bar

let bar (n: Int): String = n .foo
//                         ^^^^^^ Error: expected String, got Int
