# Function

The function syntax is a core feature of KK for creating NLL programs.

In order to create NLL programs, you will need the ability to create prefix,
infix, postfix and mixfix functions.

## Function declaration

To declare a function, we have to declare the parameter types and return types.

### Prefix function

```kk
let say (word: String): () = word .print

entry
  say "Hello world"
```

### Postfix function

```kk
let (n: Int) .factorial: Int =
  if (n < 2) then {
    1
  }
  else {
    n - 1 .factorial * n
  }

entry
  10 .factorial .print
```

### Infix function

```kk
let (a: Boolean) .and (b: Boolean): Boolean =
  (a, b) .(
    \(True, True) -> True
    \_ -> False
  )

entry
  True .and False .print
```

### Mixfix function

Mixfix function is created using keywords. Keywords are identifiers which are
not surrounded by parenthesis and also not the function name.

```kk
let (x: Int) .between (y: Int) and (z: Int): Boolean =
  x < y .and { y < z }

entry
  1 .between 2 and 3 .print
```

In this example, the function name is `between`, and the keyword is `and`.

Another example, in `f a (b) c d`, `f` is the function name, `b` is the variable `b`,
while `a`, `c` and `d` are keywords.

## Function call parsing rule

Function call in KK are left-associative, for example, `a .b c .d e` means `((a .b c) .d e)`.

## Currying

All functions in KK are curried by default.
