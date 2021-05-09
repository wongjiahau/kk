enum Nat {
  Zero,
  Succ(Nat)
}

let foo = (n: Nat): Boolean => 
  switch (n) {
    case Zero | Zero: true
            //  ^^^^ Error: unreachable case
  }