// No error
type People = {
  firstName: String,
  lastName: String
}

let s = (firstName: String, lastName: String): People -> {...}

"Adam".s("James").print()