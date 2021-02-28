# base file for testing import
# should have no error
export let foo: Integer = 2
export enum Color = Red Green
export type People = {name: String}

export let bomb
  : | String => String
  = | _  => "Hello"

export let bomb 
  : | Boolean => String
  = | _  => "World"

let spam: Integer = 3