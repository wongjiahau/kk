class Boolean = true | false

class Nat = zero | succ(Nat)

let (nat: Nat) .is even: Boolean =
  nat .(
    -> zero [ true ]
    -> succ(n) [ n .is odd ]
  )

let (nat: Nat) .is odd: Boolean =
  nat .
    -> zero [ false ]
    -> succ(n) [ n .is even ]

entry
  let one = succ(zero);
  zero .is odd .print; // false
  let two = succ(one);
  one .is even .print; // false
  two .is even .print; // true
  two .is odd .print; // false
  two .succ .is odd .print // true
