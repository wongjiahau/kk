type Grade = #a | #b | #c
class Boolean = true | false

let (a : Boolean) not : Boolean = 
  {
  | true -> false
  | false -> true
  } (a)

let<T> (left : T) `!=` (right : T) : Boolean 
  | [ equals : T -> T -> Boolean ]
  = left equals (right) not

entry
  #a `!=` (#a).print
// Error, missing [ equals : Grade -> Grade -> Boolean ]
