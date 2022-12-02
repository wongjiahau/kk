let (x: String) + (y: String): String = "${x}${y}"

entry
  let f = + "hi";
  let g = "hi" +;
  f "yo" .print; // "yohi"
  g "yo" .print; // "hiyo"
  ()
