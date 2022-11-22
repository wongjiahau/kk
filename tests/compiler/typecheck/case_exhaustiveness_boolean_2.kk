type Boolean = case true case false

let (x : Boolean) and (y: Boolean) : Boolean =
  {
    \(x = true, y = true) -> true
    \(x = false, y = true) -> false
  } (x, y)
