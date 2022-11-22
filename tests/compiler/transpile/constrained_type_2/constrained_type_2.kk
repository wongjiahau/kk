// Constraint with type variables
type List<T> = 
  case nil 
  case cons #{ head : T, tail : List<T> }

type Color = 
  case red 
  case green

type Boolean = 
  case true
  case false

let (left : Color) equals (right : Color) : Boolean = 
  {
    case #{ left = red, right = red } -> true
    case #{ left = green, right = green } -> true
    case _ -> false
  }
  #{ left, right } 

let (left : Boolean) and (right : Boolean) : Boolean = 
  {
    case #{ left = true, right = true} -> true
    case _ -> false
  } #{ left, right }

let<T> (left : List<T>) equals (right : List<T>) : Boolean 
  exists (equals: T -> T -> Boolean)
  = {
    case #{ left = nil, right = nil } -> true

    case #{ left = cons (left), right = cons (right) } ->
      left .head equals (right .head) and (left .tail equals (right .tail))

    case _ -> false
  } #{ left, right }


entry 
  let xs = cons #{ head = red, tail = cons #{ head = green, tail = nil } };
  let ys = cons #{ head = green, tail = nil };
  xs equals (xs) print; // true
  xs equals (ys) print // false
