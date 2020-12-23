enum List<T> = 
  #nil
  #cons({current: T, next: List<T>})

let bomb = 
  \#nil => true
  \#cons({current, next: #nil}) => true
