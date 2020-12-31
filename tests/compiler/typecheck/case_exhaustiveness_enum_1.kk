enum List<T> = 
  #nil
  #cons({current: T, next: List<T = T>})

let bomb = 
  \#nil => true
  \#cons({current = _, next = #nil}) => true
