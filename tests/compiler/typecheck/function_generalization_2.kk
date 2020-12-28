let f = \{x, y = {z}} => {x, z}
let x: {x: string, z: number} = {x = 'hi', y = {z = 3}}.f()
let y: {x: string, z: number} = {x = 2, y = {z = 3}}.f()
