let identity = \x => x
let constant = \_ => 2
let map = \(x, f) => x.f()
let x: String = "hello".map(identity)
let y: String = "hello".map(\_ => "yo")
let z: String = "hello".map(constant)
