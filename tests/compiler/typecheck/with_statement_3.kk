let lift = (x: Int): {x: Int} => {x}

{
  with x = 123.lift
              // Error, lift is not a satisfactory bind function
  x.print()
}