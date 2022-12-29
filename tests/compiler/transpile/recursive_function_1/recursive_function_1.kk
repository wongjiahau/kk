enum Nat { zero, succ (Nat) };

let ((x : Nat).add (y : Nat) : Nat) =
  (x, y).[(zero, n) -> n, (succ (n), a) -> n.add (a).succ];

(one = zero.succ; one.add (one).print)
