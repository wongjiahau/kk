class Option<T> = Some(T) | None 

entry
  let isSomeSome = \x ->
    x .(
      \ Some(Some(_)) -> 1
    );
  ()
  
