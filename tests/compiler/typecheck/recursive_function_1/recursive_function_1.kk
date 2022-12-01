class List<T> = nil | cons {head: T, tail: List<T>}

let <T, U> map (xs: List<T>) (f: T -> U) : List<U> =
  xs. (
    \nil -> nil
    \cons { head, tail } -> cons{ head = head .f, tail = tail .map (\t -> { t = t .f }) }
                                                     //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Error: expected List<U>, but got List<{t: U}>
  )
