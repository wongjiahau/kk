let<A> identity (a: A): A = a

let<A> constant (_: A) : Int = 2

let<A, B> map (a: A) (f: A -> B) : B = a .f

entry 
  let x: String = "hello" .map (identity);
  let y: String = "hello" .map (\_ -> "yo");
  let z: String = "hello" .map (constant);
                //^^^^^^^^^^^^^^^^^^^^^^^ Expected String, got Int
  ()
