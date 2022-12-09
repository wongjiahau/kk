class Nat =  zero | succ(Nat)

let foo (n : Nat) : Int =
  n .(     
    -> zero | zero [ 1 ]
         // ^^^^ Error: unreachable -> 
 )
