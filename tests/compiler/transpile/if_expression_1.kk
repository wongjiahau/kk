(> = (x :: float) -> (y :: float) -> javascript . "_0 > _1"),
(grade enum a , b , c , failed),
((to_grade :: float -> grade) = score -> 
  failed unless 
    (a if score > 80.0),
    (b if score > 60.0),
    (c if score > 40.0)
  ),
(85.0 |> grade , print),
(65.0 |> grade , print),
(print : 1 + sin : 2)

; (f : x) means call f with the value f
(
  (x |> fs) macro
    fs match
      (a (infix : meta id ",") b) -> `((~a : ~x) |> ~b),
      (a -> `(~a : ~x))
      
)
