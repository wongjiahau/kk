let f = \{x, y = {z}} => {x, z}
let x: {x: String, z: Number} = {x = 'hi', y = {z = 3}}.f()
let y: {x: String, z: Number} = {x = 2, y = {z = 3}}.f()
