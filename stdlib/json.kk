import "./base.kk" { 
  Result 
  Option
  map 
  ok
  all_ok
  to_json_string
  indexed
  append
  to_string
  map_result
  should_be
}

enum Json = Json 
let parse_json
  : | String => Result<Json String>
  = @@@
    (input) => {
      try {
        var result = JSON.parse(input)
        return {$: "Ok", _ : result}
      }
      catch(error) {
        return {$: "Error", _: error.toString()}
      }
    }
  @@@

enum JsonObject = JsonObject
type DecodeResult<T> = Result<T [String]>
type JsonDecoder<T> = | Json => DecodeResult<T>

let is_integer
  : | Json => Option<Integer>
  = @@@
    (json) => Number.isInteger(json)
      ? {$: "Some", _: json}
      : {$: "None"}
  @@@

export let integer
  : JsonDecoder<Integer>
  = |json => json.is_integer.(
      | Some(value) => Ok(value)
      | None => Error([
        "Expected Integer, but got #{json.to_json_string}"
      ])
    )


let is_boolean
  : | Json => Option<Boolean>
  = @@@
    (json) => typeof json === 'boolean'
      ? {$: "Some", _: json}
      : {$: "None"}
  @@@

export let boolean
  : JsonDecoder<Boolean>
  = |json => json.is_boolean.(
      | Some(value) => Ok(value)
      | None => Error([
        "Expected Boolean, but got #{json.to_json_string}"
      ])
    )

let is_array
  : | Json => Option<[Json]>
  = @@@
    (json) => Array.isArray(json)
      ? {$: "Some", _: json}
      : {$: "None"}
  @@@

export let array<A> 
  : |JsonDecoder<A> => JsonDecoder<[A]>
  = |item_decoder => |json => 
    json.is_array.(
      |None => Error([ "Expected array, but got #{json.to_json_string}" ])
      |Some(json_array) => json_array.indexed.map_result(|{index value} =>
        value.item_decoder.(
          |Ok(value) => Ok(value)
          |Error(errors) => Error(
            errors.append(
              "At array index #{index.to_string} of #{json_array.to_json_string}"
            )
          )
        )
      )
    )
      
let is_object
  : | Json => Option<JsonObject>
  = @@@
    (json) => typeof json === 'object' && !Array.isArray(json) && json !== null
      ? {$: "Some", _: json}
      : {$: "None"}
  @@@

let get_property
  : | JsonObject String => Option<Json>
  = @@@
    (json, property_name) => {
      var value = json[property_name]
      return value !== undefined ? {$: "Some", _: value} : {$: "None"}
    }

  @@@

export let required<A>
  : | String JsonDecoder<A> => JsonDecoder<A>
  = | property_name item_decoder => | json =>
    json.is_object.(
    | None => Error(["Expected object, but got #{json.to_json_string}"])
    | Some(json_object) => json_object.get_property(property_name).(
      | None => Error(["Cannot get required property '#{property_name}' of #{json_object.to_json_string}"])
      | Some(json) => json.item_decoder.(
        | Error(errors) => Error(errors.append(
            "At property '#{property_name}' of #{json_object.to_json_string}"
        ))
        | Ok(value) => Ok(value)
        )
      )
    )

export let optional<A>
  : | String JsonDecoder<A> => JsonDecoder<Option<A>>
  = | property_name decoder => | json =>
    json.is_object.(
    | None => Error(["Expected object, but got #{json.to_json_string}"])
    | Some(json_object) => json_object.get_property(property_name).(
      | None => Ok(None)
      | Some(json) => json.decoder.(
        | Error(errors) => Error(errors.append(
            "At property '#{property_name}' of #{json_object.to_json_string}"
        ))
        | Ok(value) => Ok(Some(value))
        )
      )
    )
      

export let bind<A B>
  : | JsonDecoder<A> | A => JsonDecoder<B> => JsonDecoder<B>
  = | decoder f => | json =>
      json.decoder.(
      | Ok(value) => json.(value.f)
      | Error(errors) => Error(errors)
      )

###
This is actually the `return` function in terms of monad.
###
export let as_decoder<A>
  : | A => JsonDecoder<A>
  = | value => | _ => Ok(value)


let my_decoder
  : JsonDecoder<{ x: Integer y: Boolean z: Option<Boolean> }>
  = let/bind x = "x".required(integer)
    let/bind y = "y".required(boolean)
    let/bind z = "z".optional(boolean)
    {x y r: z}.as_decoder

export let decode_json<T>
  : | JsonDecoder<T> String  => DecodeResult<T>
  = | decoder input  =>
    input.parse_json.(
      | Ok(json) => json.decoder
      | Error(error) => Error([error])
    )

do
  `1`.should_be(`1`)


  let result = 
    let/ok result = my_decoder.array.decode_json("[{\"x\": 1, \"y\": true}]")
    Ok(result)
  result.print