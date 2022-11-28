// I think a separator operator is necessary to reduce usage of parentheses 
(
  area: {
    (side,) 'square' (): 
      side * side,

    (width, height) 'rectangle' ():
      width * height
  },

  (side: 10) 'square' () | area |.print,
  
  (width: 7, height: 3) 'rectangle' () | area |.print,
)
