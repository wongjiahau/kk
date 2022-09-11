{ 
  let bar 
    :  (o: { f: (n: { x: Int }) -> Int }) -> Int
    = props -> {
      let { f } = props
      {x: "hi"}.f()
    }
}
