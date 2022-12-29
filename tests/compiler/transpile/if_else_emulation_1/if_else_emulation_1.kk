enum Boolean { True, False };

let (a b => if (condition : Boolean) (t : () -> A) else (f : () -> A) : A) =
  condition.[True -> t (), False -> f ()];

if (True) ["Hello world".print] else ["Bye".print];

if (False) ["Hello world".print] else ["Bye".print]
