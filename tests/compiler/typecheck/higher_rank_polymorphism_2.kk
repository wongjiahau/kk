do {
  let id
    : (a: Integer) => Integer 
    = <T>(t: T): T => t
       // ^^^ Error: expected zero type variable, but got one
}