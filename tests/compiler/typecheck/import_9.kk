// testing import all kind of symbols
import {foo as bar, Color, bomb} from "./import_1.kk"

do bar.print()

let x: Color = Red
do x.print()
do Green.print()
do true.bomb().print()
do "hi".bomb().print()