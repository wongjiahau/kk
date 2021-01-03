enum List<Element> =
  #nil
  #cons({
    current: Element,
    next: List<Element= Element>
  })

let foo = \#cons({
  current = 1,
  next = #cons({
    current = true,
    next = #nil
  })
}) => 0
