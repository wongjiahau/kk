interface Equatable<T> {
}

let notEquals = <T> where Equatable<A>(a: T, b: T): Boolean => true
//                                  ^ Error: Unknown type variable
