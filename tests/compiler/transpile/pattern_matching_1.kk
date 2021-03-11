let and
  : | Boolean Boolean => Boolean
  = | true true => true
    | _ _ => false 

do true.and(true).print
do true.and(false).print
do false.and(true).print
do false.and(false).print
