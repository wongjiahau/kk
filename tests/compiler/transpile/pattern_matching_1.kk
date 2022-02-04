{
  and: x: y: {x, y} match [
    {x: true, y: true}: true,
    _ : false
  ],
  false and true | print,
  true and false | print,
  x: 1 + 2, 
}
