type Option<T> { Some(T), None }

{
  let isSomeSome = x ->
    switch x {
      case Some(Some(_)) : true
    }
}
