# This is a comment
# Rule, all binary operations are left-associative
# All operators whose names starts with a colon is right-associative, and has a
# lower precedence than usual binary operations.
# Names are either alphanumeric+underscore OR non-whitespace-symbols

# Record
# Record is also constructed as binary trees,
# this property allows library authors to create DSL easily without using helper-functions / wrappers / extension-methods / macros
# Record syntax is {EXPR (KEY EXPR)+}
{
  [
    ("hello world" | print)
  ]

  # Assignment
  # Assignment is done using record construction
  # Example
  x 3

  # Function construction (abstraction)
  f (x: y: x + y)

  # Function deconstruction (application)
  # (a) Binary function call
  demoBinaryFunctionCall (x f y g y)

  # Since binary operation are left-associative, the above equals to:
  demoBinaryFunctionCall2 ((x f y) g y)

  # (b) Unary function call
  # Unary function call can be done by using the left-associative pipe operator,
  # or using parenthesis
  unary ("hello" ++ "world" | print)

  # or 
  unary2 (print ("hello" ++ "world"))

  car {_ name "toyota" age 3 sayHello ({}: print "hello")}

  # Record access
  name (car.name) # "toyota"

  # To access the first element
  first (car.0) # {}

  # example DSL using records
  (n between {lower and upper}) (n >= lower and (n <= upper))

  # sample array manipulation
  [1 2 3] map (+ 1) filter (> 2) reduce {0 using +}

  answer (1 between {3 and 4} | print)

  # Record update
  newCar (car . {_ name (car . name | toUppercase)})

  # Record spread
  profile1 {_ name "lee" age 3}
  profile2 {_ ...profile1 job "none"} # {name "lee" age "3" job "none"}

  # Record punning
  demo {$ x 1 y 2 result {$ x , y ,} }

  # Array
  names ["bob" "bartholomew" "chungus"]

  # Module
  # Every file is a value
  x (import "./yo.kk")

  # Import all from a module can be done using record spread
  ... (import "./yo.kk")

  # Keywords starts with $ (for now)
  x $ok
  

  # Union type is formed by using record OR keywords
  (type (Ok Result Fail)) [
    {$ ok Ok} # Note that the space between $ and ok is important
    {$ fail Fail}
  ]

  (myResult : (Int Result String)) {$ ok 1234}

  # Tagged union (polymorphic variants)
  (type (List A)) [
    $nil
    {(List A) cons A}
  ]

  # Example DSL using union
  (date input) (input match {
    _
    {n days $later}
      (now + (n * day))

    $today 
      now
      
    $yesterday
      (now - day)

  } | (Date.fromInt) where {
    _
    now (System.currentDate {})
    day (24 * hour)
    hour (60 * minute)
    minute (60 * second)
    second (1000 * millisecond)
    millisecond 1
  })

  _ ({7 days $later} | date | print)

  # Conditionals
  grade ((score: Float): [
    (score >= 80: A)
    (score >= 60: B)
    (score >= 40: C)
    (score >= 20: D)
  ] else F)

  grade (score: Float) := {
    score >= 80: A,
    score >= 60: B,
    score >= 40: C,
    score >= 20: D,
  } else F

  grade (score match {
    F
    (score >= 80) A
    (score >= 60) B
    (score >= 40) C
    (score >= 20) D
  } given (score: Float))

  main [
    ([1 2 3] map (x + 1 given x))
  ]

  # Pattern matching
  length (case [
    0
    (_ Cons xs) (xs | length + 1)
  ])
}


