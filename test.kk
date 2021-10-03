type Shape<T> = {
  area: (t: T) => String
}
let area = <T>(t: T, {...}: Shape<T>): String => t.area()

enum Circle { Circle({radius: Float}) }
enum Square { Square({side: Float}) }
let circleArea: Shape<Circle> = {
  area: (_)  => "Im circle"
}
let squareArea: Shape<Square> = {
  area: (_) => "Im square"
}

enum List<T> {
  Nil,
  Cons({head: T, tail: List<T>})
}

let map = <A, B>(xs: List<A>, f: (a: A) => B): List<B> =>
  switch (xs) {
    case Nil: 
      Nil
    case Cons({head, tail}): 
      Cons({head: head.f(), tail: tail.map(f)})
  }

let join = (xs: List<String>, delimiter: String): String =>
  switch (xs) {
    case Nil: 
      ""
    case Cons({head, tail: Nil}):
      head
    case Cons({head, tail}):
      "${head}${delimiter}${tail.join(delimiter)}"
  }

let toString = <T>(list: List<T>, stringify: (t:T) => String): String =>
  "[ ${list.map(stringify).join(", ")} ]"

enum Color {
  Red,
  Green
}

let toString = (color: Color): String =>
  switch color {
    case Red: "Red"
    case Green: "Green"
  }

Cons({
  head: Cons({head: Red, tail: Cons({head: Green, tail: Nil})}),
  tail: Nil
})
.toString(.toString(toString)).print()


let foldLeft = <A, B>(list: List<A>, init: B, f: (accumulator: B, element: A) => B): B => 
  switch list {
    case Nil: 
      init
    case Cons({head, tail}):
      tail.foldLeft(init.f(head), f)
  }

enum Option<T> {
  Some(T),
  None
}

let iff = <T>(option: Option<T>, condition: Boolean, value: T): Option<T> =>
  switch option {
    case Some(value):
      Some(value)
    case None:
      switch condition {
        case true:
          Some(value)
        case false:
          None
      }
  }

let elsee = <T>(option: Option<T>, value: T): T =>
  switch option {
    case Some(value): value
    case None: value
  }

let lessThan = (a: Integer, b: Integer): Boolean => @@@ _0 < _1 @@@

enum Grade {
  A,
  B,
  C,
  F
}

let grade = (score: Integer): Grade =>
  None
    .iff(score.lessThan(40), F)
    .iff(score.lessThan(60), C)
    .iff(score.lessThan(80), B)
    .elsee(A)

  case
    .if(=> score.<(40)) then(=> F)
    .if(=> score.<(60)) then(=> C)
    .if(=> score.<(80)) then(=> B)
    .else(=> A)


80.grade().print()
60.grade().print()
40.grade().print()
20.grade().print()

let compose 
  : <A, B, C>(f: A => B, g: B => C) => (A => C)
  = (f, g) => x => x.g().f()