let lift (x : Int) : (x : Int) = (x,)

entry
  ~(
    print (123 ! lift) 
              // ^^^^ Error, lift is not a satisfactory bind function
  )
