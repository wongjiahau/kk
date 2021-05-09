// Subpatterns
let isVowel = (c: Character): Boolean =>
  switch (c) {
    case 'a' | 'e' | 'i' | 'o' | 'u':
      true
    case _:
      false
  }

'a'.isVowel().print()
'e'.isVowel().print()
'i'.isVowel().print()
'o'.isVowel().print()
'u'.isVowel().print()

'b'.isVowel().print()
'z'.isVowel().print()