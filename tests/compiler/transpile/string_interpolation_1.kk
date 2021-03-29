let uppercase
  : | String => String
  = @@@ (s) => s.toUpperCase() @@@

let lowercase
  : | String => String
  = @@@ (s) => s.toLowerCase() @@@

let my_dog
  : String
  = "bibi"

do "Hello #{my_dog.uppercase} where are you #{my_dog.lowercase}?".print