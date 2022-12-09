class Bool = True | False

let <A> if (condition: Bool) then (t: () -> A) else (f: () -> A) : A =
  condition .     
    -> True [ t () ]
    -> False [ f () ]

entry
  if (True) then [ "Hello world" .print ] else [ "Bye" .print ]; // Hello world
  if (False) then [ "Hello world" .print ] else [ "Bye" .print ] // Bye
