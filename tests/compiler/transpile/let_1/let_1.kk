(enum Null (Null))

(do
  (let foo = (-> Null 123))
  (let xs =
    (do
      (let x = (. Null foo))
      (let y = (. (# (x = (. Null foo))) x))
      (# x y)))
  (. xs print))
