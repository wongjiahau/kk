do
  let bar = 
    | [] => true
    | [true .._] => false
    | [false .._] => false
    | _ => false
  null