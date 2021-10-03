[
  more_than = (x : float) -> (y : float) -> javascript <-> "_0 > _1",
  grade enum [a, b, c, failed],
  (to-grade : float -> grade) = score -> 
    failed unless [
      a if score > 80.0,
      b if score > 60.0,
      c if score > 40.0,
    ],

  85.0 |> [grade, print],
  65.0 |> [grade, print],
]
let more_than = (x: Float, y: Float): Boolean => @@@ _0 > _1 @@@

enum Grade { A, B, C, Failed }

let grade = (score: Float): Grade =>
  if (score.more_than(80.0)) A
  else if (score.more_than(60.0)) B
  else if (score.more_than(40.0)) C
  else Failed


85.0.grade().print()
65.0.grade().print()
45.0.grade().print()
35.0.grade().print()
