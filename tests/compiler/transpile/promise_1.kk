###
Return a given integer after 0.1 second.
###
let gimme 
  : | Integer => !{result: Integer}
  = @@@ (n) => new Promise(resolve => setTimeout(() => resolve({$result: n}), 100)) @@@

let hello
  : ![Integer]
  = 
    let! {result: a} = 1.gimme
    let! b = 2.gimme
    let result = [a b.result]
    let _ = result.print
    !result