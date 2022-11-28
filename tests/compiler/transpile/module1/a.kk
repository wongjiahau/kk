let a : Int = 1

entry
  // b is defined in b.kk
  // b can be used without import

  // because every source file in the same folder can access each other's
  // non-private symbols

  // by default, symbol without access modifiers are protected
  b.print
