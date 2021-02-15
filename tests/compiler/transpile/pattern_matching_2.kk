enum Bool = True() False()
let and = | x y => 
  let True() = x
  let True() = y
  True()

do True().and(True()).print()
do True().and(False()).print()
do False().and(True()).print()
do False().and(False()).print()
