let foo 
  : | Null => { x: Integer }
  = | _ => {x: 123}

do null.foo.x.print