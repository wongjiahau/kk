entry 
  let a = (x = 2, y = (z = 3));
  let b = a.(b = 3); // 'b' is a non-existing property
  ()
