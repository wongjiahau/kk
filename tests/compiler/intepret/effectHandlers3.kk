// async await
type Promise<A> = (A -> ()) -> () 

effect Async<A> = [
  await (promise: Promise<A>) : A
]
  
let run <A>(f: () -> () effect (Async<A>)): A =
  f () 
    effect (Async) [
      await (promise) = promise(resume)
    ]

entry
  run {
    let x = `tests/compiler/intepret/basic1.kk` readFile await;
    let y = `tests/compiler/intepret/effectHandlers3.kk` readFile await;
    x print;
    y print
  }
