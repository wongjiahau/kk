effect Exception = [
  raise <A>(code: Int) : Int
]

entry
  let x : String = 123 ! raise;
  x print
