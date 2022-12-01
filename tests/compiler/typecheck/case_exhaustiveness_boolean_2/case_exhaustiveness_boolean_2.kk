class Boolean = true | false

let (x : Boolean) .and (y: Boolean) : Boolean =
  { x, y } .( 
    \{x = true, y = true} -> true
    \{x = false, y = true} -> false
  )
