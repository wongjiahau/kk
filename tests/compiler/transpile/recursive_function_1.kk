enum Nat = 
  Zero
  Succ(Nat)

let add
  : | Nat Nat => Nat 
  = | Zero n => n
    | Succ(n) a => Succ(n.add(a))

let one: Nat = Succ(Zero)
do one.add(one).print()