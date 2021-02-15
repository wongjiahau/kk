let foo = 
  | [] => true
  | [_ ...[]] => false
