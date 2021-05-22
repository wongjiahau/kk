interface Equatable<T> {
  let equals: (left: T, right: T) => Boolean
}

enum Grade { A, B, C }

A.equals(A).print()
// Error, missing implementation