type Nat = \ zero \ succ(Nat)| succ(Nat)

let foo (n : Nat) : Int =
  {
    \zero | zero -> 1
          // ^^^^^ Error: unreachable \
  } (n)
