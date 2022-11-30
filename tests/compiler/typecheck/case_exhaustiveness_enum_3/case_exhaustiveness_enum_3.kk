type Option<T> { Some(T), None }

{
  let isSomeSome = x ->
    switch x {
      \ Some(Some(_)) : true
    }
}
