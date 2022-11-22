let (s: String) .replace first (old: String) with (new: String) : Int = 1
let (s: String) .replace each (old: String) with (new: String) : Int = 2

entry
  "Hello" .replace first "l" with "k" .print; // 1
  "Hello" .replace each "l" with "k" .print // 2


