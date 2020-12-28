enum Option<T> = #some(T) #none

let tail2 = \xs =>
  let [_, ...[_, ...tail2]] = xs 
  tail2

do [1,2,3].tail2().print()
do [].tail2().print()

