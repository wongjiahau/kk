// testing import all kind of symbols
module (foo = bar, Color, bomb) = import "./import_1.kk"

entry
  bar print;
  let x: Color = #red;
  x print;
  #green print;
  bomb () print;
  "hi" bomb print;
