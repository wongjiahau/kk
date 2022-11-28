// Example from http://craftinginterpreters.com/resolving-and-binding.html
(
  a: 1,
  innerScope: (
    showA: {a |.print},
    () | showA, // should be 1
    a: 2,
    () | showA, // should be 2
  ),
)

