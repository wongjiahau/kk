class Grade = A | B | C
class Boolean = true | false

let (a : Boolean) .not : Boolean = 
  a. (
    -> true [ false ]
    -> false [ true ]
  )

let<T> (left : T) .`!=` (right : T) : Boolean 
  given { equals : T -> T -> Boolean }
  = left .equals (right) .not

entry
  A .`!=` (A) .print
// Error, missing { equals : Grade -> Grade -> Boolean }
