function and (x: Boolean, y: Boolean): Boolean 
  switch ({ x, y }) {
    case { x: true, y: true }: true
    case { x: false, y: true }: false
  }