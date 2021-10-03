// every operator is right-associative, including meta-level operators
// a ~ b ~ c 
// a ~ (b ~ c)
// (f / x) / y === y \ x \ f === x f y
(List / a) :: nil | (List/a) ; a

x = 1 ; 2 ; 3 ; nil
// the above is equivalent to below
(x = (1 ; (2 ; (3 ; nil))))

even = n -> 0 == n % 2
// the above syntax is equivalent to below
(even = (n ~ (0 == (n % 2))))

map 
  : a => b => (List / a) ~ (a ~ b) ~ List / b
  = (nil ~ _ ~ nil) | (x ; xs) ~ f ~ (f / x) ; xs map f

filter
  : a => (List / a) ~ (a ~ Boolean) ~ (List / a)
  = (nil ~ _ ~ nil)
  | (x ; xs) ~ p ~ (p / x) ? (F ~ xs filter p) | T ~ x ; xs filter p


