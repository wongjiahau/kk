let s
  : | Null => String
  = | null => 
    let x = {..} // Error: missing type annotation
    x