(enum Boolean True False)

(defn
  (forall (a)
    if (condition Boolean)
    :then (t (-> () a))
    :else (f (-> () a))
    a)
  (match condition (True (t ())) (False (f ()))))

(if True :then (-> _ (print "Hello world"))
  :else (-> _ (print "Bye")))

(if False :then (-> _ (print "Hello world"))
  :else (-> _ (print "Bye")))
