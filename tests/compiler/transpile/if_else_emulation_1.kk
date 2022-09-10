type Bool
  = #true
  | #false

type List<A>
  = #nil
  | #cons (current: A, next: List<A>)

type Option<A>
  = #none
  | #some(A)

let <A, B> (a: A) match (f: A -> B): B = a f

let <A, B> (xs: List<A>) `find map` (f: A -> Option<B>): Option<B> = 
  xs match {
    #cons (current, next) -> current f match {
      #some(o) -> #some(o),
      #none -> next `find map` (f)
    },
    #nil -> #none
  }

type Branch<A> = (condition: () -> Bool, body: () -> A)

let <A> (xs: List<A>) push (x: A): List<A> = 
  xs match {
    #nil -> #cons (current = x, next = #nil),
    #cons (current, next) -> #cons (current, next = next push (x))
  }

let <A> if (condition: Bool) (body: () -> A): List<Branch<A>> =
  #nil push (condition = { condition }, body)

let <A> (branches: List<Branch<A>>) elif (condition: () -> Bool) (body: () -> A): List<Branch<A>> = 
  branches push (condition, body)

let <A> (branches: List<Branch<A>>) else (body: () -> A): A =
  branches 
    `find map` { branch -> 
      branch.condition () match {
        #true -> 
          #some (branch.body ()),

        #false -> 
          #none
      }
    } 
    match {
      #some (body) -> body,
      #none -> body ()
    }

let <A> (branches: List<Branch<A>>) end: Option<A> =
  branches `find map` { branch -> 
    branch.condition () match {
      #true -> #some (branch .body ()),
      #false -> #none
    }
  } 

entry
  if (#true) { "Hello world" print } else { "Bye" print }; // Hello world

  if (#false) { "Hello world" print } elif { #true } { "Mid" print } else { "Bye" print }; // Mid

  if (#false) { "Hello world" print } elif { #false } { "Mid" print } else { "Bye" print }; // Bye

  if (#false) { 1 } end print; // #none
  if (#true) { 1 } end print // #some(1)
