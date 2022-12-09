class Boolean = true | false

let (x: Boolean) .and (y: Boolean): Boolean = 
  { x, y } .(
    -> { x = true, y = true } [ true ]
    -> _ [ false ]
  )

entry
  true .and (true) .print;
  true .and (false) .print;
  false .and (true) .print;
  false .and (false) .print
