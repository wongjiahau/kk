let identity = \x => x
let constant = \x => 2
let map = \(x, f) => x.f
let x: string = 'hello'.map(identity)
let y: string = 'hello'.map(\a => 'yo')
let z: string = 'hello'.map(constant)
