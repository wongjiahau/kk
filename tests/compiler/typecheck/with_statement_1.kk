// Expects no error
enum Option<T>{
  Some(T),
  None
}

let map = <A, B>(this: Option<A>, f: (a: A) => Option<B>): Option<B> => {
  switch (this) {
    case Some(x): x.f()
    case None: None
  }
}

export let sum = (a: Option<Integer>, b: Option<Integer>): Option<{ a: Integer, b: Integer }> => {
  with a = a.map;
  with b = b.map;
  Some({ a, b })
}

do Some(1).sum(Some(2)).print()
do None.sum(Some(2)).print()
do Some(1).sum(None).print()
do None.sum(None).print()