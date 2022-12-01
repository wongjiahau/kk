class List<T>
  = nil
  | cons { current: T, next: List<T> }

let<T, U> bomb (xs: List<T>) (f: T -> U): Int =
  xs .( 
    \nil -> 1
    \cons { current = _, next = nil } -> 0
 )

