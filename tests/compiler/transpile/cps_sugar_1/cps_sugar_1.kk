enum (Option T) { some (T), none };

let (T U => (option : Option T).unwrap (callback : T -> Option U) : Option U) =
  option.[some (t) -> callback (t), none -> none];

(
  x =
    unwrap ~
      some
        {
          x = ? some "hi",
          y = some 2.?,
          z = unwrap ~ some { x = none.?, y = some 2.? }
        };
  """some {x = "hi", y = 2, z = none}""" x.print
)
