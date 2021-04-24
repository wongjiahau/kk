type People = {
  first_name: String
  last_name: String
}

let s
  : | String String => People
  = | _ last_name => {..} 
                    // Error: missing property first_name

do "Adam".s("James").print