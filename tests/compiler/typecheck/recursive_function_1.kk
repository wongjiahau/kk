let arrayLength = 
  \[] => 0
  \[_, ...tail] => 1.add(tail.arrayLength())
