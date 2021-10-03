enum Stream<T> { 
  Cons({
    value: T, 
    next: (null: Null) => Stream<T>
  })
}

let plus = (a: Integer, b: Integer): Integer => @@@ _0 + _1 @@@

let minus = (a: Integer, b: Integer): Integer => @@@ _0 - _1 @@@

let equals = (a: Integer, b: Integer): Boolean => @@@ _0 === _1 @@@

let stream = (n: Integer): Stream<Integer> => 
  Cons({
    value: n,
    next: (_) => n.plus(1).stream()
  }) 

enum List<T> {
  Nil,
  Cons({ head: T, tail: List<T> })
}

let head = <T>(Cons({...}): Stream<T>): T => value
let tail = <T>(Cons({...}): Stream<T>): Stream<T> => null.next()

let take = <T>(stream: Stream<T>, n: Integer): List<T> =>
  if(n.equals(0))
    Nil
  else 
    Cons({ 
      head: stream.head(),
      tail: stream.tail().take(n.minus(1))
    })

let toString = (n: Integer): String => @@@ JSON.stringify(_0) @@@

type Stringifiable<T> = {
  toString: (t: T) => String
}

let innerToString = <T>(list: List<T>, {...}: Stringifiable<T>): String =>
  switch(list) {
    case Nil: ""
    case Cons({...}): "${head.toString()}, ${tail.innerToString({...})}"
  }
  
let toString = <T>(list: List<T>): String => 
  "[${list.innerToString({...})}]"



0.stream().take(1000).toString().print()