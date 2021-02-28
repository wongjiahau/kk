enum List<Element> =
  Nil
  Cons({
    current: Element
    next: List<Element>
  })

let xs: Integer = 
  let x = Cons({
    current: 1
    next: Cons({
      current: true
      next: Nil
    })
  })
  123
