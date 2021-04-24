let lift
  : | Integer => [Integer]
  = | x => [x]

do 
  let/lift x = 123
     // Error, lift is not a satisfactory bind function
  null