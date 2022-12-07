# Expression

## Tagged union

```kk
class List<T> = Nil | Cons {current: T, next: List<T>}
```

## Record/Object

```kk
{ x = 1, y = "Hello" }
```

## Let

```kk
let x = 3;
x + 1
```

## Statements

Statements-expression are separated by semicolons, and the last expression will
be the value that is "returned".

```kk
let x =
  "Hello" .print;
  123

```

## Tuple

```kk
(1, 2, "Hello")
```

## String

```
"Hello world"
```

String can also be interpolated using `${}`:

```kk
let x = "world";
"Hello ${x}"
```
