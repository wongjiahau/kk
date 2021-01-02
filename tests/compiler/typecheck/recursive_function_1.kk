enum List<Of> = 
    #nil
    #cons({current: Of, next: List<Of= Of>})

let listMap<From, To>
 : \(List<Of= From>, \From => To) => List<Of= To>
 = 
  \(#nil, _) => #nil
  \(#cons({current, next}), f) => #cons({
    current = current.f(), 
    next = #cons({current= 0, next= #nil}).listMap(f)
  })
