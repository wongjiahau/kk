enum Nat =
  Zero()
  Succ(Nat)

let add: -> (Nat, Nat) [ Nat ] =
  -> (Zero(), n) => n
  -> (n, Zero()) => n
  -> (Succ(n), a) => Succ(n.add(a))

do "Hello world".print()

let one = Succ(Zero())
let two = Succ(one)
let three = Succ(three)
do Zero.add(Zero()).print()