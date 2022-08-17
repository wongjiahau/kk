// Using prelude symbols imported from other file should not have errors

module [showMe] = import "./import_1.kk" 

"Hello world".showMe()
