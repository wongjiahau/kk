{
  hello: 
    99 
    ! raise
    <- x,

  raiseConst:
    (1 | hello) 
      handle {
        raise: 42 <- message,
      },

  raiseConst | print, // should be 42 not 50
}
