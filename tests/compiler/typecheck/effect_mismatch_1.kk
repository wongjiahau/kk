effect Exception = [
  raise <A>(message: String) : A
]

// Error, expected Int, but got Int ! (Exception)
let f (message: String) : Int = 
  message ! raise
