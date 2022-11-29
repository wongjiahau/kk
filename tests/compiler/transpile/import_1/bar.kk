// Using prelude symbols imported from other file should not have errors

import "./foo" {
  show
}

entry
  "Hello world" .show
