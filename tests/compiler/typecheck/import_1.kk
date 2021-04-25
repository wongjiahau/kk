// base file for testing import
// should have no error
export enum Color {Red, Green}
export type People = {name: String}

export let bomb = (_: String): String  => "Hello"

export let bomb = (_: Boolean): String => "World"

export let foo = 123
let spam = 123