import "./base.kk" { 
  Result
  characters
  reduce
  is_alphanumeric
}

export enum Json =
  Array(Json)
  String
  Number
  Boolean(Boolean)
  Null
  Object([{key: String value: Json}])

export let parse_json
  : | String => Result<Json String>
  = | input => input.characters

enum Token =
  RightCurlyBracket
  LeftCurlyBracket
  LeftSquareBracket
  RightSquareBracket
  Colon
  Comma
  StringLiteral
  NumberLiteral
  NullLiteral
  BooleanLiteral

let tokenize
  : | String => Result<[Token] String>
  = | input => 
      let/loop {tokens characters} = {tokens: [] characters: input.characters}
      characters
      
      input.characters.reduce({
        from: []
        with: | tokens character =>
          character.(
            '{' => RightCurlyBracket
            '}' => LeftCurlyBracket
            '[' => LeftSquareBracket
            ']' => RightSquareBracket
            ',' => Comma
            _ => 
              if character.is_alphanumeric

          )
      })
