(
  hello: {x: 99 ! raise},

  raiseConst:
    (1 | hello) 
      handle (
        raise: {message: 1 + message},
      ),

  raiseConst | print, // should be 100 not 99
)

