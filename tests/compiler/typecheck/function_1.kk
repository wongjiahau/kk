// Error: function name overlap with property name
let name
  : | {name: String} => String
  = | _ => "foo"