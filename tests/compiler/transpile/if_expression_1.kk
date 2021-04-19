function more_than(x: Float, y: Float): Boolean
  @@@ _0 > _1 @@@

enum Grade { A, B, C, Failed }

function grade(score: Float): Grade 
  if score.more_than(80.0) 
    A
  else if score.more_than(60.0) 
    B
  else if score.more_than(40.0) 
    C
  else 
    Failed


do 85.0.grade.print()
do 65.0.grade.print()
do 45.0.grade.print()
do 35.0.grade.print()
