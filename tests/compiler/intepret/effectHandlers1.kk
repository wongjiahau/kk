{
  hello = { x -> 99 ! raise },

  raiseConst = hello 1 handle #{
    raise = { message -> 1 + message }
  },

  raiseConst print // should be 100 not 99
} ()


