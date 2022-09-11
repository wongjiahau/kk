type Option<T> = #Some(T) | #None

let foo (x: Option<Int>): Int =
  let #Some(x) = x;
  x
