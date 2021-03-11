enum Nat = Zero Succ(Nat)

let equals
  : | Nat     Nat     => Boolean
  = | Zero    Zero    => true
    | Zero    Succ(_) => false
    | Succ(_) Zero    => false
    | Succ(a) Succ(b) => a.pred.equals(b.pred)

let pred
  : | Nat => Nat
  = | Zero => Zero
    | Succ(n) => n

let one: Nat = Succ(Zero)
let is_even
  : | Nat => Boolean
  = | Zero => true
    | n => n.pred.is_odd

let is_odd
  : | Nat => Boolean
  = | Zero => false
    | n => n.pred.is_even

do Zero.is_odd.print # false
do one.is_even.print # false
do Succ(one).is_even.print # true
do Succ(one).is_odd.print # false
do Succ(Succ(one)).is_odd.print # false