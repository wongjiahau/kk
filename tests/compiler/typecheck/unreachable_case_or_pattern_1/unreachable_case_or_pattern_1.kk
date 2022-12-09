class Nat = zero | succ(Nat)

let foo (n : Nat) : Int =
  n.(
    -> succ(succ(zero)) [ 1 ]
    -> succ(zero | succ(zero)) [ 0 ]
//   ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error: partially unreachable -> , namely `succ(succ(zero))`
  )
