type List<T> {
  Nil,
  Cons({head: T, tail: List<T>})
}

let map = <T, U>(xs: List<T>, f: (t: T) -> U): List<U> ->
  switch xs { 
    case Nil: 
      Nil
    case Cons({head, tail}): 
      Cons({head: head.f(), tail: tail.map(t -> {t: t.f()})})
                              //  ^^^^^^^^^^^^^^^^^^^^^^^^^ Error: expected List<U>, but got List<{t: U}>
  }