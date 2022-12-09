class Nat = zero | succ(Nat)

let (x : Nat) .add (y : Nat) : Nat =
  {x, y} .
    -> { x = zero, y = n } [ 
      n 
    ]
    -> { x = succ(n), y = a } [ 
      n .add (a) .succ 
    ]

entry
  let one = zero .succ;
  one .add (one) .print

