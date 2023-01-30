(enum Nat Zero (Succ Nat))

(defn
  ((x Nat) add (y Nat) Nat)
  (match
    (% x y)
    ((% (x Zero) y) y)
    ((% (x (Succ n)) y) [. n (add y) Succ])))

(do (let one (. Zero Succ)) [. one (add one) print])
