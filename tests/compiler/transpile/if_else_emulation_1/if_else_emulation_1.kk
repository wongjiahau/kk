(enum Boolean (True False))

(def
  if
  (forall (a) {-> Boolean :then (-> () a) :else (-> () a) a})
  {-> condition :then t :else f (match condition (True (t ())) (_ (f ())))})

(if True :then (-> _ (print "Hello world")) :else (-> _ (print "Bye")))

(if False :then (-> _ (print "Hello world")) :else (-> _ (print "Bye")))
