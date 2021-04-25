enum Option<T> { Some(T), None }

do {
  let isSomeSome = x =>
    switch x {
      case Some(Some(_)) : true
    }
}
