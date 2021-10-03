("./stream.kk" import { })

// This makes parsing easier, thus may make auto complete easier to implement, pretty print easier
(factorial 
  : (int -> int) 
  = (n -> (n <= 1 ? [(true -> 1) (false -> (n - 1 |> factorial * n)]))))

(def factorial 
  (type (-> int int))
  (body (-> n (if (<= n 1) 1 (* n (factorial (n - 1)))))))

(boolean = (true | false))
(list = (t => (nil | (t :: (t list.)))))

(length 
  : (t => (t list; -> int))
  = (xs -> (xs match [
      (nil -> 0)
      (x :: xs -> (xs length. + 1))])))

(factorial :: (int -> int) = (n -> (n <= 1 ? {
  true -> 1
  false -> 
})))

(0 |> factorial |> print)
(print (factorial 0))
(print (factorial 0))
(print (factorial 0))

(n.:(int)).factorial.->(int)
  .=(n.->(
    n.<(1).
      if(true) then(1)
      if(false) then(n.*(n.-(1).factorial))
  ))