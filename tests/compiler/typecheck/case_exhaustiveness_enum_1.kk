enum List<T> = 
  Nil
  Cons({current: T next: List<T>})

let bomb<A>
  : | List<A> => Boolean
  = | Nil => true
    | Cons({current: _ next: Nil}) => true
