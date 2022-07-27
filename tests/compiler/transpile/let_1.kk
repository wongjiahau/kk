enum Nulls = #null

entry
  let foo = { #null -> 123 };
  let xs = (
    let x = #null foo;
    let y = { .x = #null foo }.x;
    [x, y]
  );
  xs print
