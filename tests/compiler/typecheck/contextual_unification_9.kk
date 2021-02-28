let bar 
  : | { f: | { x: Integer } => Integer } => Integer
  = | props =>
  let { f } = props
  {x: "hi"}.f()

