// Nested pattern
type Vehicle = case car(Color) case motor

type Color = 
  case white 
  case gold
  case black

type Boolean = case true case false

let [is shiny car]: Vehicle -> Boolean = {
  case car(white | gold) -> true
  case _ -> false
}

entry
  white car [is shiny car] print;
  gold car [is shiny car] print;
  black car [is shiny car] print;
  motor [is shiny car] print
