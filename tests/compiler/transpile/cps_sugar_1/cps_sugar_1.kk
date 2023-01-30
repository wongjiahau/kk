(enum (Option t) (some t) none)

(defn
  (forall (a b) (option (Option a))
    unwrap (callback (-> a (Option b))) (Option b))
  (match option ((some t) (callback t)) (none none)))

(do
  (let x
    (~ unwrap
      (some
        (%
          (x [. "hi" some !])
          (y [. 2 some !])
          (z
            (~ unwrap (some (% (x [. none !]) (y [. 2 some !])))))))))
  /* (some (% (x "hi") (y 2) (z none))) */
  [. x print])
