enum List<Of> = 
    Nil()
    Cons({current: Of, next: List<Of= Of>})

let listMap<From, To>
 : \(List<Of= From>, \From -> To) -> List<Of= To>
 = 
  \(Nil(), _) => Nil()
  \(Cons({current, next}), f) => Cons({
    current = current.f(), 
    next = Cons({current= 0, next= Nil()}).listMap(f)
  })
