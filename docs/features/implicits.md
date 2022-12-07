# Implicits

Implicit is a feature for interface-programming.

It is done using the `given` keyword.

For example:

```kk
class Boolean = true | false
class Grade = a | b

let (left : Grade) .equals (right : Grade) : Boolean =
  { left, right } .(
    \{ left = a, right = a } -> true
    \{ left = b, right = b } -> true
    \_ -> false
  )

let (g : Boolean) .not : Boolean =
  g. (
    \true -> false
    \false -> true
  )

let<T> (left : T) .not equals (right : T) : Boolean
  given { equals: T -> T -> Boolean }
  = left .equals (right) .not

entry
  a .equals (a) .print; // true
  a .not equals (b) .print; // true
  a .equals (b) .print // false
```
