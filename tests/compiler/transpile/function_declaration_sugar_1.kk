let prefix (a: Integer): Integer = a

let prefix2 (a: Integer) (b: Integer): Integer = b

let (a: Integer) postfix: Integer = a

let (a: Integer) infix (b: Integer): String = 
  a print;
  b print;
  'hello'

let (a: Integer) infix2 (b: Integer) (c: Integer): String = 
  a print;
  b print;
  c print;
  'hello'

entry
  prefix 1 print;
  prefix2 3 4 print;
  5 postfix print;
  6 infix 7 print;
  8 infix2 9 10 print

  
