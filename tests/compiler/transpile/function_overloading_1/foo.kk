(defn replace ((s String) :first (old String) :with (new String) Int) 1)

(defn replace ((s String) :each (old String) :with (new String) Int) 2)

(defn map (forall (A B) (a A) (f (-> A B)) B) (f a))

[. "Hello" (replace :first "l" :with "k") print]

[. "Hello" (map replace :first "l" :with "k") print]

[. "Hello" (replace :each "l" :with "k") print]

[. "Hello" (map replace :each "l" :with "k") print]
