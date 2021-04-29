// testing import all kind of symbols
import {foo as bar, Color, bomb} from "./import_1.kk"

bar.print()

let x: Color = Red
x.print()
Green.print()
true.bomb().print()
"hi".bomb().print()