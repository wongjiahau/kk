# testing import all kind of symbols
import "./import_1.kk" { 
  foo: bar 
  Color 
  bomb 
}

do bar.print()

let x: Color = Red
do x.print()
do Green.print()
do true.bomb().print()
do "hi".bomb().print()