
type Option<T> = case some(T) case none

let<A, B> (o: Option<A>) map (f: A -> B) : Option<B> =
  {
    case some(o) -> o f some
    case none -> none
  } (o)

entry
  ~("%${none ! map}%") print;
  ~("%${some("hello world") ! map}%") print

