enum List<T> {
  Nil,
  Cons({head: T, tail: List<T>})
} 

{
  let bomb = list => 
    switch (list) {
      case Nil: true
      case Cons({head: true, tail: _}): true
    }
}
