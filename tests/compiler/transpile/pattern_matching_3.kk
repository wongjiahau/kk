// Using Javascript's keyword as property name should not fail 

{
  let foo = { class } => class
  { class: "Hello" }.foo().print()
}
