enum Option [T] {
  some(T),
  none
};

foo (x: Option [Int]): Int = (
  some(x) = x;
  x
)
