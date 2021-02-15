// This test should fail

enum Option<T> = Some(T) None()

let baz = | a => 
  let Some(Some(b)) = a // This should implicitly return Option<Option<T>>
  Some([b]) // This should fail with: Expected Option<Option<T>>, but got Option<[T]>