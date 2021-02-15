// This test should result in no compile error
enum Option<T> = Some(T) None()

let bar = | a =>
  let Some(x) = a // This should implicitly return Option<T>, not Option<String>
  Some([x])