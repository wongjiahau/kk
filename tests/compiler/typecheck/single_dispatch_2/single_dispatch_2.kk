let foo (_: String): String = "Hello"

let foo (): String = "World"

let main (): Int = {
  let f = { \x -> x foo };
  123
}
