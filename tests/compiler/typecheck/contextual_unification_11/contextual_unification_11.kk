// Contextual unification for If-expression
class Boolean = true | false

let <A> if (condition: Boolean) then (success: () -> A) else (failure: () -> A) : A =
  condition .(
    \true -> success ()
    \false -> failure ()
  )

entry
  let x: { a: String } = if (true) then {{a = 2}} else {{a = "hello"}};
  //                                                         ^^^^^^^ Expected Int, but got String
  ()
