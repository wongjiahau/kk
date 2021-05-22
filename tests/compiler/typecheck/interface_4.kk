interface Stringifiable<T> {
  let toString: (t: T) => String
}

implements Stringifiable<String> {
  // Error: missing implementation `toString`
}