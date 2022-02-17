// Example from http://craftinginterpreters.com/resolving-and-binding.html
{
  a: 1,
  innerScope: {
    showA: #: a | print,
    # | showA,
    a: 2,
    # | showA,
  },
}

