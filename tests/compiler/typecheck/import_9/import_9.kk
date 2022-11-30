// testing import all kind of symbols
import "./import_1.kk"

entry
  let x: Color = #red;
  x.print;
  #green.print;
  bomb ().print;
  "hi" bomb.print

