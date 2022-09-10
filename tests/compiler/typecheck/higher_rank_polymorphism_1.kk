let {id} = {
  id: <T>(t: T): T => t
}

{
  let x: String = "Hello".id() // No error
  let y: Int = 123.id() // No error
  let z: String = 123.id() 
  //              ^^^^^^^^ Error: expected String, got Int
}