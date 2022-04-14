{
  run: `` 
    | f 
    handle {
        ? : n match {
             a `ok` ``:
             a | resume,
             e `err` ``: e `err` ``,
          } <- n,
    }
    <- f,

  ("oh no" `err` `` ! ?) + (2 `ok` `` ! ?) <- `` | run | print,

  (1 `ok` `` ! ?) + (2 `ok` `` ! ?) <- `` | run | print,
}
