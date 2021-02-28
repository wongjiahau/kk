enum List<T> = 
  Nil
  Cons({current: T next: List<T>})

do
  let bomb = 
    | Nil => true
    | Cons({current: true next: _}) => true 
  null
