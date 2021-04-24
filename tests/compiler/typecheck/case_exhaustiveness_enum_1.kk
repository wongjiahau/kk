enum List<T> {
  Nil,
  Cons({current: T, next: List<T>})
}

let bomb = <A>(xs: List<A>): Boolean => 
  switch (xs) {
    case Nil: true
    case Cons({current: _, next: Nil}): true
  }