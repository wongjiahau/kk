{ 
  // This is inspired by APL Over operator (https://aplwiki.com/wiki/Over)
  over: f: g: a: b: (a | g) f (b | g),                                                                                                   
  square: a: a * a,

  // Sum of squares
  3 (+ over square) 4 | print,

  // Product of squares
  3 (* over square) 4 | print,

}
