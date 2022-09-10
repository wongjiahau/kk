enum List<T>
  = #nil
  | #cons { current: T, next: List<T> }

let bomb(xs: List<T>) (f: T -> U): Boolean =
  {
  | #nil -> true
  | #cons { .current = _, .next = #nil } -> true
  } xs

