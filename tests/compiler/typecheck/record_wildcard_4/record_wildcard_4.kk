type People = { 
  firstName: String,
  lastName: String
}

let (x: String) .s (lastName: String): People =
  {..}
// ^^ Error: missing property firstName
