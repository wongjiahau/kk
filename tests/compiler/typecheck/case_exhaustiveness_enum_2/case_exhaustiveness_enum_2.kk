class Boolean = True | False
class List<T> = Nil | Cons {head: T, tail: List<T>}

entry
  let bomb =
    -> Nil [ True ]
    -> Cons {head = True, tail = _} [ True ]
    ;
  ()

