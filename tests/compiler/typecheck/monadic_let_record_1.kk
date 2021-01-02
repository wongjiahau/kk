let f = \x => 
  let {a} = x
  a

let result: string = {a = 2}.f()
