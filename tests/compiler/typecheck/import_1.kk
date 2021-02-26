# base file for testing import
# should have no error
export let foo = 2
export enum Color = Red Green
export type People = {name: String}
export let bomb = |_:String  => "Hello"
export let bomb = |_:Boolean  => "World"
let spam = 3