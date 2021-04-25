enum Nat { 
  Zero,
  Succ(Nat)
} 

let add = (x: Nat, y: Nat): Nat => 
  switch({x, y}) {
    case {x: Zero, y: n}: 
      n
    case {x: Succ(n), y: a}: 
      Succ(n.add(a))
  }


do {
  let one = Succ(Zero);
  one.add(one).print()
}