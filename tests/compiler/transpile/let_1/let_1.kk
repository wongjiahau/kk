type Null = case null
entry
  let foo = { case null -> 123 };
  let xs = (
    let x = null foo;
    let y = #{x = null foo} .x;
    #(x, y)
  );
  xs print
