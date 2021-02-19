enum List<Of> = 
  Nil
  Cons({current: Of next: List<Of>})

let list_map<From To> : | List<From> | From -> To -> List<To> = 
  | Nil _ => Nil
  | Cons({current, next,}) f => Cons({
      current current.f()
      next Cons({current 0 next Nil}).list_map(f)
  })