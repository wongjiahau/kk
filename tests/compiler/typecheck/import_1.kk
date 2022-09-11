// base file for testing import
// should have no error
type Color = #Red | #Green
type People = (name: String)

let bomb (_: String): String  = "Hello"

let bomb (): String = "World"

let foo : Int = 123
private let spam : Int = 123
