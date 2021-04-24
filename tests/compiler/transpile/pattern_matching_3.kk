// Using Javascript's keyword as property name should not fail 

do {
  let foo = { with } => with;
  { with: "Hello" }.foo().print()
}
