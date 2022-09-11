type Nat = #zero | #succ(Nat)

let (x : Nat) add (y : Nat) : Nat =
  {
    (x = #zero, y = n) -> n,
    (x = #succ(n), y = a) -> #succ(n add (a))

  } (x, y)


entry
  let one = #succ(#zero);
  one add (one) print

