// This is for testing:
// Entry points should only be transpiled at entry points

module (foo) = import "./no_entry_no_do_2.kk"

entry
  "World" print
