
type Option<T> = #some(T) | #none

let<A, B> (o: Option<A>) map (f: A -> B) : Option<B> =
  {
    \#some(o) -> o f #some
    \#none -> #none
  } (o)

entry
  ~("%${#none ! map}%") print;
  ~("%${#some("hello world") ! map}%") print

