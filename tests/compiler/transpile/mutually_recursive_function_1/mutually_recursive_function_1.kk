type Boolean = case true case false

type Nat = case zero case succ(Nat)

let [is even]
  : Nat -> Boolean
  = {
    case zero -> true
    case succ(n) -> n [is odd]
  }

let [is odd]
  : Nat -> Boolean
  = {
    case zero -> false
    case succ(n) -> n [is even]
  }

entry
  let one = succ(zero);
  zero [is odd] print; // false
  let two = succ(one);
  one [is even] print; // false
  two [is even] print; // true
  two [is odd] print; // false
  succ(two) [is odd] print // true
