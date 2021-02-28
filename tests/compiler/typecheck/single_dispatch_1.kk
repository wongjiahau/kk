let foo
  : | String => String
  = | _ => "hello"
let foo<T> 
  : | T => String
  = | _ => "hello"