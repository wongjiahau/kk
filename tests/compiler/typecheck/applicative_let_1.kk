// Expects no error
enum Option<T> = Some(T) None

let map<A B>
  : | Option<A> | A => Option<B> => Option<B>
  = | Some(x) f => x.f
    | None _ => None

export let sum
  : | Option<Integer> Option<Integer> => Option<[Integer]>
  = | a b => 
      let/map x = a
      let/map y = b
      Some([x y])

do Some(1).sum(Some(2)).print
do None.sum(Some(2)).print
do Some(1).sum(None).print
do None.sum(None).print