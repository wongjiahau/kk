enum List<T> = 
  Nil
  Cons({current: T next: List<T>})

let bomb = 
  | Nil => true
  | Cons({current: true next: _}) => true 
