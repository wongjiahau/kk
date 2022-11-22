let <T> foo (t: T): Int = 123

entry
  let z = 3;
  let x = #{ x = 0 foo, y = "456", z };
  x print
