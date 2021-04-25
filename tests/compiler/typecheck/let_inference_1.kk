do {
  let f = {x, y: {z}} => {x, z};
  //       ^ Initially we are not sure what is the type of x, so we assign it an implicit type variable, say T1


  let x = {x: "hi", y: {z: 3}}.f();
  //          ^^^^ With this, we know the type of x should be String


  let y = {x: 2, y: {z: 3}}.f()
  //          ^ Error: this should be String

}
