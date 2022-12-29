class Future<T> = Future((T -> ()) -> ())

let (filename: String) .read: Future<String> = Future -> callback [ intrinsic read_file (filename, callback) ]

let <T, U> (Future(future): Future<T>) .await (f: T -> Future<U>): Future<U> =
  Future -> c [
    future  -> t [
      let Future(callback) = f (t);
      callback(c)
    ]
  ]

let <T> lift (t: T): Future<T> = Future -> c [ c (t) ]

entry
  let file1 = "./tests/compiler/transpile/cps_sugar_3/spongebob.txt";
  let file2 = "./tests/compiler/transpile/cps_sugar_3/patrick.txt";
  let Future(f) = ~await (lift {
    spongebob = file1 .read!,
    patrick = file2 .read!
  });
  f (print)
