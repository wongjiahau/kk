let identity = <A>(a: A): A => a

let constant = <A>(_: A): Integer => 2

let map = <A, B>(a: A, f: (a: A) => B): B => a.f()

do {
  let x: String = "hello".map(identity);
  let y: String = "hello".map((_) => "yo");
  let z: String = "hello".map(constant)
}
