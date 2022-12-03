let lift (x : Int) : { x : Int } = { x, }

entry
  ~lift (
// ^^^^ Error, lift is not a satisfactory bind function
   print (123 !) 
  )
