type Foo = #foo(Bar) | #nullFoo 

type Bar = #bar(Foo) | #nullBar

entry
  #foo(#bar(#foo(#nullFoo))) print // Compile Error
