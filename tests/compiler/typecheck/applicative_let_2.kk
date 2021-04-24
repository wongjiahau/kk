// Expects no error
enum Option<T> {
  Some(T), 
  None
}

let map = <A, B>(o: Option<A>, f: (a: A) => Option<B>): Option<B> =>
  switch(o) {
    case Some(o): o.f()
    case None: None
  }

export let foo
  : | Option<{bar: String}> => Option<String>
  = | a =>
      let/map x = a
      Some(x.bar)

do Some({bar: "hello world"}).foo.print
do None.foo.print