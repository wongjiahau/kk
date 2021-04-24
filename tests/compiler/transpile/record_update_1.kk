type R = {
  x: Integer,
  y: {
    z: Integer
  }
}
do {
  let a = { x: 2, y: { z: 3 } };
  let b = a.{ x: 3, y.{ z: 4 }};
  a.print();
  b.print()
}