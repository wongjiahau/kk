(defn
  ((s String) replace :first (old String) :with (new String) Int)
  1)

(defn
  ((s String) replace :each (old String) :with (new String) Int)
  2)

(defn (forall (A B) (a A) map (f (-> A B)) B) (f a))

[. "Hello" (replace :first "l" :with "k") print]

[. "Hello" (map replace :first "l" :with "k") print]

[. "Hello" (replace :each "l" :with "k") print]

[. "Hello" (map replace :each "l" :with "k") print]
