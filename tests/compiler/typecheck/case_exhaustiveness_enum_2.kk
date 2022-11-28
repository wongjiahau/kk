type List<T> {
  Nil,
  Cons({head: T, tail: List<T>})
} 

{
  let bomb = list -> 
    switch (list) {
      \ Nil: true
      \ Cons({head: true, tail: _}): true
    }
}
