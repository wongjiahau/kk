{
  hello: 
    99 
    ! raise
    <- x,

  raiseConst:
    (1 | hello) 
      handle {
        raise: 1 + message <- message,
      },

  raiseConst | print, // should be 100 not 99
}
