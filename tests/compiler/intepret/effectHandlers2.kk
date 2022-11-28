(
  run: {
    f: () | f handle (
      ? : {
        n: n | { 
          a 'ok' (): a | resume, 
          e 'err' (): e 'err' () 
        }
      }
    )
  },

  { (`oh no` 'err' () ! ?) + (2 'ok' () ! ?) } | run |.print,
  { (1 'ok' () ! ?) + (2 'ok' () ! ?) } | run |.print,
)

