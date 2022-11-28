type Nat = \ zero \ succ(Nat)| succ(Nat)

let foo (n : Nat) : Int =
  {
    \succ(succ(zero)) -> 1
    \succ(zero | succ(zero)) -> 0
//   ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error: partially unreachable \, namely `succ(succ(zero))`
  }
