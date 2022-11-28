class Option<T> = some (T) | none

let<T, U> (option: Option<T>) .unwrap (callback : T -> Option<U>) : Option<U> = 
  option .(
    \some (t) -> callback (t)
    \none -> none
  )

entry
  let x = ~(
    some {
      x =  some "hi" ! unwrap, 
      y = some 2 ! unwrap, 
      z =  ~(some { x = none ! unwrap, y = some 2 ! unwrap })
    }
  );
  x .print // some {x = 1, y = 2, z = none}

