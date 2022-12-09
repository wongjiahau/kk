class Foo = a { x: String, y: String } | bala

entry 
  bala .(
    -> a { x, y } | bala [ true ]
            //    ^^^^ Error: missing binding `x`
  )
