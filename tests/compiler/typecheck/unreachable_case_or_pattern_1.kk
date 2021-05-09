enum Nat {
  Zero,
  Succ(Nat)
}

let foo = (n: Nat): Boolean => 
  switch (n) {
    case Succ(Succ(Zero)): true
    case Succ(Zero | Succ(Zero)): false
    //   ^^^^^^^^^^^^^^^^^^^^^^^ Error: partially unreachable case, namely `Succ(Succ(Zero))`
  }