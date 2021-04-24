enum Option<T> = Some(T) None
do
  let foo = 
    | Some(1) => Some("hi")
    | Some(_) => Some("lol")
    | Some(_) => None // This case is unreachable
    | None => None
  null