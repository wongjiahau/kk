// Nested pattern
enum Vehicle = #car(Color) | #motor

enum Color = #white | #gold | #black

let "is shiny car"
  : Vehicle -> Boolean
  = {
  | #car(#white | #gold) -> true
  | _ -> false
  }

entry
  #car(#white) "is shiny car" print;
  #car(#gold) "is shiny car" print;
  #car(#black) "is shiny car" print;
  #motor "is shiny car" print
