class Foo = foo(Bar) | nullFoo 

class Bar = bar(Foo) | nullBar

entry
  foo(bar(foo(nullFoo))) .print // Compile Error
