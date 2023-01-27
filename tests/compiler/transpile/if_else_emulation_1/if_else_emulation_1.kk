(enum Boolean (True False))

(defn if
  (forall (a) (condition Boolean) :then (t (-> () a)) :else (f (-> () a)) a)
  (match condition (case True -> (t ())) (case False -> (f ()))))

(if True :then (-> _ (print "Hello world")) :else (-> _ (print "Bye")))

(if False :then (-> _ (print "Hello world")) :else (-> _ (print "Bye")))
