enum List<Element> {
  Nil,
  Cons({
    current: Element,
    next: List<Element>
  })
}

do {
  let foo = Cons({
    current: 1,
    next: Cons({
      current: true,
      next: Nil
    })
  }) => 0
}