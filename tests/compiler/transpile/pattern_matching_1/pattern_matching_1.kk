type Boolean = case true case false

let (x: Boolean) and (y: Boolean): Boolean = 
  {
    case #{ x = true, y = true } -> true
    case _ -> false
  } #{ x, y } 

entry
  true and (true) print;
  true and (false) print;
  false and (true) print;
  false and (false) print
