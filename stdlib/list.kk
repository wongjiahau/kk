import "./base.kk" {
  should_be
  reduce
  reverse
  plus
  to
  max
  map
  Option
  loop
  Continuation
  more_than
}

enum List<T> = 
  Nil
  Cons({head: T tail: List<T>})

let length<A>
  : | List<A> => Integer
  = | xs =>
    let/loop {count node} = {count: 0 node: xs}
    node.(
      | Nil => Break(count)
      | Cons({head: _ tail}) => Continue({count: count.plus(1) node: tail})
    )
    
let reduce<A B>
  : | List<A> {from: B with: | B A => B} => B
  = | xs {..} =>
    let/loop {result node} = {result: from node: xs}
    node.(
      | Nil => Break(result)
      | Cons({..}) => Continue({result: result.with(head) node: tail})
    )

###
Reverse a list.
```
`[1 2 3].to_list.reverse`.should_be(`[3 2 1].to_list`)
```
###
let reverse<A>
  : | List<A> => List<A>
  = |.reduce({from: Nil with: | tail head => Cons({..})})

###
Apply a function to every element of a list.
```
`[1 2 3].to_list.map(|.plus(2))`.should_be(`[3 4 5].to_list`)
```
###
let map<A B>
  : | List<A> | A => B => List<B>
  = | xs f => xs
      .reverse
      .reduce({ from: Nil with: | result x => Cons({head: x.f tail: result})})

###
Remove elements from a list using the given predicate.
```
`[1 2 3].to_list.filter(|.more_than(2))`.should_be(`[3].to_list`)
```
###
let filter<A>
  : | List<A> | A => Boolean => List<A>
  = | xs predicate => xs
      .reverse
      .reduce({ 
        from: Nil
        with: | result x => x.predicate.(
          | true => Cons({head: x tail: result})
          | false => result
        )
      })

let to_list<A>
  : | [A] => List<A>
  = | xs => xs.reverse.reduce({from: Nil with: | list x => Cons({head: x tail: list})})

###
Get the last element of a list.
```
`[].to_list.last`.should_be(`None`)
`[1 2 3].to_list.last`.should_be(`Some(3)`)
```
###
let last<A>
  : | List<A> => Option<A>
  = | Nil => None
    | Cons({head tail: Nil}) => Some(head)
    | Cons({head: _ tail}) => tail.last

###
Get the first element of a list.
```
`[].to_list.first`.should_be(`None`)
`[1 2 3].to_list.first`.should_be(`Some(1)`)
```
###
let first<A>
  : | List<A> => Option<A>
  = | Nil => None
    | Cons({head tail: _})  => Some(head)

(123).print