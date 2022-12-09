class List<Element> = Nil | Cons { current: Element, next: List<Element> }

entry
  let foo = -> Cons {
    current = 1,
    next = Cons {
      current = true,
      next = ()
      //     ^^ Expected Int, got ()
    }
  } [ 0 ];
  ()
