type Boolean = case true case false
let (a : String) `not equals` (b : String) : Boolean 
  exists (equals : T -> T -> Boolean)
 //                ^ Error: Unknown type symbol
  = true
