// Expects no error
type Option<T>{
  Some(T),
  None
}

let map = <A, B>(this: Option<A>, f: (a: A) -> Option<B>): Option<B> -> {
  switch (this) {
    case Some(x): x.f()
    case None: None
  }
}

export let sum = (a: Option<Int>, b: Option<Int>): Option<{ a: Int, b: Int }> -> {
  with a = a.map
  with b = b.map
  Some({ a, b })
}

Some(1).sum(Some(2)).print()
None.sum(Some(2)).print()
Some(1).sum(None).print()
None.sum(None).print()