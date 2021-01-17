enum Food = Nut()
enum Tool = Nut()

let f = \x =>
  let Food::Nut() = x
  2

let x = Tool::Nut()

do x.f().print()
