let bar:  { o: { f: { n: { x: Int } } -> Int } } -> Int
  = -> props [
    let { f } = props;
    //    ^ Error: no such property
    //    In Haskell, we would have gotten a longer error message as follows,
    //    due to bottom-up type inference:

    //    Expected { o: { f: { n: { x: Int } } -> Int } } -> Int
    //    But got <A> { f: {x: String} -> A } -> A
    {x = "hi"} .f
  ]
