class List<T> = Nil | Cons { current: T, next: List<T> }

let xs : Int =
  let x = Cons { 
    current = 1,
    next = Cons {
      current = (),
      next = Nil
    }
  };
  123
