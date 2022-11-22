type Nat = case zero case succ(Nat)| succ(Nat)

let foo (n : Nat) : Int =
  {
    \succ(succ(zero)) -> 1
    \succ(zero | succ(zero)) -> 0
//   ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error: partially unreachable case, namely `succ(succ(zero))`
  }
