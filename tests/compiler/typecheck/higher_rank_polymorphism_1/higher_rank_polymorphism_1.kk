let <T> id (t: T): T = t

entry 
  let foo = { id, };
  let x: String = "Hello" .(foo .id); // No error
  let y: Int = 123 .(foo .id); // No error
  let z: String = 123 .(foo .id);
  123
  //                   ^^^^^^^^^^ Error: expected String -> String, got Int -> Int
