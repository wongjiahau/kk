# Expects no error
enum Option<T> = Some(T) None

let map<A B>
  : | Option<A> | A => Option<B> => Option<B>
  = | Some(x) f => x.f
    | None _ => None

export let foo
  : | Option<{bar: String}> => Option<String>
  = | a =>
      let/map x = a
      Some(x.bar)

do Some({bar: "hello world"}).foo.print
do None.foo.print