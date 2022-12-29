enum Boolean { true, false };

enum Grade { a, b };

let ((left : Grade).equals (right : Grade) : Boolean) =
  (left, right).[(a, a) -> true, (b, b) -> true, false];

let ((g : Boolean).not : Boolean) = g.[true -> false, true];

type (Eq T) = T -> T -> Boolean;

let (T => { equals : Eq T } | (left : T).not equals (right : T) : Boolean) =
  left.equals (right).not;

"""
true
"""
a.equals (a).print;

"""
true
"""
a.not equals (b).print;

"""
false
"""
a.equals (b).print
