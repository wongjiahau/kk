class Boolean = true | false
let and (x: Boolean) (y: Boolean): Boolean =
  {x, y} .(
    \{ x = true, y = true } -> true
    \_ -> false
    \_ -> false
  )
