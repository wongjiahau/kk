
type Foo = #A(x: String, y: String) | #B(x: String, y: String, z: String)

entry 
  {
    \#A(x, y) | #B(x, y, z) -> 1
        //               ^ Error: extraneous binding `z`
  } (#A(x = "hi", y= "bye"))
