type Nat = case zero case succ(Nat)

let (x : Nat) add (y : Nat) : Nat =
  {
    case #{ x = zero, y = n } -> n
    case #{ x = succ(n), y = a } -> succ(n add (a))

  } #{ x, y }


entry
  let one = zero succ;
  one add (one) print

