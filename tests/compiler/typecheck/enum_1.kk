enum List<T> =
  #nil
  #cons({
    current: T,
    next: List<T>
  })

let xs = #cons({
  current = 1,
  next = #cons({
    current = true,
    next = #nil
  })
})
