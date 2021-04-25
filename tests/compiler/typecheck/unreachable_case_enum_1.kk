enum Option<T> { Some(T), None }
let foo = (o: Option<Integer>): Option<String> => 
  switch o { 
    case Some(1): Some("hi")
    case Some(_): Some("lol")
    case Some(_): None // This case is unreachable
    case None: None
  }