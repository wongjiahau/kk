enum List<T> = 
  #nil
  #cons({current: T, next: List<T= T>})

let bomb = 
  \#nil => true
  \#cons({current = true, next = _}) => true 
