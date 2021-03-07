# Using Javascript's keyword as property name should not fail 

let foo: | {with: String} => String =  | {with} => with

do {with: "Hello"}.foo().print()
