// Using prelude symbols imported from other file should not have errors

import "./import_1.kk" (
  [show me]
)

entry
  "Hello world" [show me]
