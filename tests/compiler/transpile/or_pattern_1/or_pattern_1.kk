// Subpatterns
class Boolean = true | false

let isVowel
  : Character -> Boolean = {
    'a' | 'e' | 'i' | 'o' | 'u'  -> true,
    _ -> false
  } 

entry
  'a' isVowel.print;
  'e' isVowel.print;
  'i' isVowel.print;
  'o' isVowel.print;
  'u' isVowel.print;

  'b' isVowel.print;
  'z' isVowel.print
