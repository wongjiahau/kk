effect Exception = [
  raise (message: String) : Int
]

entry
  "hello world" ! raise
    effect Exception [
      raise (message) = resume "yo" // Expected Int but got String
    ]
