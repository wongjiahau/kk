// Expects no error
type Option<T> = some(T) | none

let<A, B> (o : Option<A>) map (f : A -> Option<B>) : Option<B> =
  {
    \some(o) -> o f
      \none - none
  } (o)

let foo (a : Option<(bar : String)>): Option<String> = 
  ~(some((a ! map).bar))

entry
  some(bar = "hello world") foo.print;
  none fo.print
