type Boolean = case true case false
type Grade = case a case b

let (left : Grade) equals (right : Grade) : Boolean =
  {
    case #{ left = a, right = a } -> true
    case #{ left = b, right = b } -> true
    case _ -> false
  } #{ left, right }

let (g : Boolean) not : Boolean = 
  {
    case true -> false
    case false -> true
  } (g)

let<T> (left : T) [not equals] (right : T) : Boolean 
  exists (equals: T -> T -> Boolean)
  = left equals (right) not

entry
  a equals (a) print; // true
  a [not equals] (b) print; // true
  a equals (b) print // false
