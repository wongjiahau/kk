(s: String) .replace first (old: String) with (new: String) : Int = 1;
(s: String) .replace each (old: String) with (new: String) : Int = 2;

[A, B] (a: A) .map (f: A -> B) : B = f (a);

"Hello" .replace first "l" with "k" .print; // 1
"Hello" .map(replace) first "l" with "k" .print; // 1
"Hello" .replace each "l" with "k" .print; // 2
"Hello" .map(replace) each "l" with "k" .print // 2
