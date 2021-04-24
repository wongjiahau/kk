// No error
type People = {
  first_name: String
  last_name: String
}

let s
  : | String String => People
  = | first_name last_name => {..}

do "Adam".s("James").print