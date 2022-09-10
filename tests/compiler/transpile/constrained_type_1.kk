type Boolean = #true | #false
type Grade = #a | #b

let (left : Grade) equals (right : Grade) : Boolean =
  {
    (left = #a, right = #a) -> #true,
    (left = #b, right = #b) -> #true,
     _ -> #false
  } (left, right)

let (b : Boolean) not : Boolean = 
  {
    #true -> #false,
    #false -> #true
  } (b)

let<T> (left : T) `not equals` (right : T) : Boolean 
  exists (equals: T -> T -> Boolean)
  = left equals (right) not

entry
  #a equals (#a) print; // true
  #a `not equals` (#b) print; // true
  #a equals (#b) print // false
