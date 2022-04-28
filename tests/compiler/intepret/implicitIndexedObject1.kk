(
  x: ('hello', my: 'beautiful', 'world'),
  x."0" | print, // 'hello'  
  x.my | print,  // 'beautiful'  
  x."1" | print, // 'world'  
)
