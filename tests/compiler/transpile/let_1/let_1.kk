enum Null {
  Null
};

(
  foo = [Null -> 123];
  xs = (
    x = Null .foo;
    y = { x = Null .foo } .x;
    { x ,y }
  );
  xs .print
)
