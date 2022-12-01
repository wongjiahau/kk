class Boolean = True | False
class List<T> = Nil | Cons {head: T, tail: List<T>}

entry
  let bomb = \list -> 
    list .(
      \Nil -> True
      \Cons {head = True, tail = _} -> True
    );
  ()

