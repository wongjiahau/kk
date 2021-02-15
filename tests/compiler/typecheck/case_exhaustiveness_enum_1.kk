enum List<T> = 
  Nil()
  Cons({current: T next: List<T T>})

let bomb = 
  | Nil() => true
  | Cons({current _ next Nil()}) => true
