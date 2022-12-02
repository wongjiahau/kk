// Cannot be overloaded
let foo (s: String): String = "Hello"
let <T> foo (t: T): String = "hello"
