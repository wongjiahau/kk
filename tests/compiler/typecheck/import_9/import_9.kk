// testing import all kind of symbols
import "../import_1"

entry
  let x: Color = Red;
  x .print;
  Green .print;
  bomb () .print;
  bomb "hi" .print

