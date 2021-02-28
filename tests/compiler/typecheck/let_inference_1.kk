do
  let f = | {x y: {z}} => {x z}
  let x = {x: "hi" y: {z: 3}}.f()
  let y = {x: 2 y: {z: 3}}.f()
  null
