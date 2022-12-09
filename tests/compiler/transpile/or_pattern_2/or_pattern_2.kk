// Nested pattern
class Vehicle = car(Color) | motor

class Color = white | gold | black

class Boolean = true | false

let (vehicle: Vehicle) .is shiny car: Boolean =
  vehicle .
    ->  car(white | gold) [ true ]
    [ false ]

entry
  white .car .is shiny car .print;
  gold .car .is shiny car .print;
  black .car .is shiny car .print;
  motor .is shiny car .print
