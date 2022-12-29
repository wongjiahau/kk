type R = { x : Int, y : { z : Int } };

(a = { x = 2, y = { z = 3 } }; b = a.{ y = a.y.{ z = 4 } }; a.print; b.print)
