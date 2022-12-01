class Option<T> = Some(T) | None

entry
  let a = Some(None);
  let x: Option<String> = a; // Error, expectd Option<String>, found Option<Option<@TVAR>>
  ()
