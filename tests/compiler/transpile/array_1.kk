let foo
  : Integer -> Integer
  = { _ -> 123 }

entry
  let xs = [ 999 foo, { .x = 1 foo }.x ];
  xs print
