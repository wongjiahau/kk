effect raise : <A> Int -> A

let hello (): Int effect (raise) = 
  raise 99;
  123



let raiseConst () = 
  hello 1 
    effect [
      raise (message) = 1 + message
    ]

entry
  raiseConst.print // should be 100 not 99

