// async await
#{
  run = {
    f -> f handle #{
      await = {
        promise -> promise resume
      }
    } ()
  },

  main = {
    x = `tests/compiler/intepret/basic1.kk` readFile ! await,
    y = `tests/compiler/intepret/effectHandlers3.kk` readFile ! await,
    x print,
    y print
  } ()
}
(
  run: {
    f: () | f handle (
      await : {
        promise: resume | promise 
      }
    )
  },

  { 
    (
      x: `tests/compiler/intepret/basic1.kk` | readFile ! await,
      y: `tests/compiler/intepret/effectHandlers3.kk` | readFile ! await,
      x | print,
      y | print
    )
  } | run,
)

