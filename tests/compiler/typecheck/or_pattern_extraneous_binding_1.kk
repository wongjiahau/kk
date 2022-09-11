
type Foo {
  A({x: String, y: String}),
  B({x: String, y: String, z: String})
}

switch(A({x: "hi", y: "bye"})) {
  case A({x, y}) | B({x, y, z}): true
        //               ^ Error: extraneous binding `z`
}