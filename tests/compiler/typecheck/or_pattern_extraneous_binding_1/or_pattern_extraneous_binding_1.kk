
class Foo = A { x: String, y: String } | B { x: String, y: String, z: String }

entry 
  A { x = "hi", y= "bye" } .( 
    -> A { x, y } | B { x, y, z } [ 1 ]
        //                  ^ Error: extraneous binding `z`
 ) 
