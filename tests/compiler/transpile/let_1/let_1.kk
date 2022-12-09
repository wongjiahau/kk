class Null = Null
entry
  let foo = -> Null [ 123 ];
  let xs = (
    let x = Null .foo;
    let y = {x = Null .foo} .x;
    { x, y }
  );
  xs .print
