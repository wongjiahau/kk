//////
This function takes anything string and output 123.
For example:
```
do null.foo.print // Compile error: expected `String` but got `Null`
```
//////
let foo
  : | String => Integer
  = | _ => 123 