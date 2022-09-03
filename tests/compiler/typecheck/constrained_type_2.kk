type Boolean = #true | #false
let (a : String) `not equals` (b : String) : Boolean 
  | [ equals : T -> T -> Boolean ]
 //            ^ Error: Unknown type symbol
  = #true
