enum Nat =
  Zero()
  Succ(Nat)

let add: | Nat Nat -> Nat =
  | Zero() n => n
  | n Zero() => n
  | Succ(n) a => Succ(n.add(a))

let one = Succ(Zero())
do one.add(one).print()