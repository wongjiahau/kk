// Expects no error
class Option<T> = some(T) | none

let<A, B> (o : Option<A>) .map (f : A -> Option<B>) : Option<B> =
  o .(
    \some(o) -> o .f
    \none -> none
  )

let foo (a : Option<{ bar : String }>): Option<String> = 
  ~(some((a ! map).bar))

entry
  some { bar = "hello world" } .foo .print;
  none .foo .print
