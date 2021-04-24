// No error
type People = {
  firstName: String,
  lastName: String
}

let toString = ({ ... }: People): String => "${firstName}, ${lastName}"

do { firstName: "Adam", lastName: "Bob" }.toString().print()