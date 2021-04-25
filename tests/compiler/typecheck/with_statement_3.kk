let lift = (x: Integer): {x: Integer} => {x}

do {
  with x = 123.lift;
              // Error, lift is not a satisfactory bind function
  x.print()
}