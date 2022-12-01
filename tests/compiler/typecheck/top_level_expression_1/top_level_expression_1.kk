let foo (): Int = 123

entry
  foo ()
  // Error: expected () but got Int 
  // This is because expressions tha are not assigned to a variable must have unit type
