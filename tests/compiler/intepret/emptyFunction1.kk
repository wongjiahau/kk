(
  f: {`hello world` |.print},

  `yoyo` |.print,

  () | f

  // Expect `yoyo` is.printed before `hello world`
)
