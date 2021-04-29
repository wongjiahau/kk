enum Option<T> { Some(T), None }

let foo = (x: Option<Integer>): Integer => {
  let Some(x) = x
  x
}