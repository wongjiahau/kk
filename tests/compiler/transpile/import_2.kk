# Using prelude symbols imported from other file should not have errors

import "./import_1.kk" { 
  show_me
}

do "Hello world".show_me