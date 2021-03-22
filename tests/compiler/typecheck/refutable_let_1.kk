enum Option<T> = Some(T) None

let foo
  : | Option<Integer> => Integer
  = | x =>
    let Some(x) = x
    x