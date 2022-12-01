class Option<T> = some(T) | none

let foo (x: Option<Int>): Int =
  let some(x) = x;
  x
