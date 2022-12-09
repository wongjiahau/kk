class Option<T> = some(T) | none 

let foo (o: Option<Int>): Option<String> =
  o . 
    -> some(1) [ some ("hi") ]
    -> some(_) [ some ("lol") ]
    -> some(_) [ none ] // This is unreachable
    -> none    [ none ] 
