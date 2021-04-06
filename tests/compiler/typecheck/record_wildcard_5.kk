let s
  : | Null => Null
  = | null => 
    let f = | {..} => null
              # Error: missing type annotation
    null