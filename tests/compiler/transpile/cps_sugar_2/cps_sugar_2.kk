
class Option<T> = some(T) | none

let<A, B> (o: Option<A>) .map (f: A -> B) : Option<B> =
  o .(     
    \some(o) -> o .f .some
    \none -> none
 )

entry
  ~map("%${none !}%").print;
  ~map("%${some "hello world" !}%").print

