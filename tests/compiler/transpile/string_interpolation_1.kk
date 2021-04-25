let uppercase = (s: String): String => @@@_0.toUpperCase() @@@

let lowercase = (s: String): String => @@@_0.toLowerCase() @@@

do {
  let my_dog = "bibi";
  "Hello ${my_dog.uppercase()} where are you ${my_dog.lowercase()}?".print()
}