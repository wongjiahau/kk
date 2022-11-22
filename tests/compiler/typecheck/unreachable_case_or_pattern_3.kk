type Nat = case zero case succ(Nat)| succ(Nat)

let foo (n : Nat) : Int =
  {
    \zero | zero -> 1
          // ^^^^^ Error: unreachable case
  } (n)
