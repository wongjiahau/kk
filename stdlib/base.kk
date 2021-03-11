# Boolean functions

###
```
do `true.not`.should_be(`false`)
do `false.not`.should_be(`true`)
```
###
export let not
  : | Boolean => Boolean
  = | true => false
    | false => true

###
```
do `true .and(true)` .should_be(`true`)
do `false.and(true)` .should_be(`false`)
do `true .and(false)`.should_be(`false`)
do `false.and(false)`.should_be(`false`)
```
###
export let and
  : | Boolean Boolean => Boolean
  = | true true => true
    | _ _ => false

###
```
do `true .or(true)` .should_be(`true`)
do `false.or(true)` .should_be(`true`)
do `true .or(false)`.should_be(`true`)
do `false.or(false)`.should_be(`false`)
```
###
export let or
  : | Boolean Boolean => Boolean
  = | false false => false
    | _ _ => true

###
```
do `true .nand(true)` .should_be(`false`)
do `false.nand(true)` .should_be(`true`)
do `true .nand(false)`.should_be(`true`)
do `false.nand(false)`.should_be(`true`)
```
###
export let nand
  : | Boolean Boolean => Boolean
  = | true true => false
    | _ _ => true

###
```
do `true .nor(true)` .should_be(`false`)
do `false.nor(true)` .should_be(`false`)
do `true .nor(false)`.should_be(`false`)
do `false.nor(false)`.should_be(`true`)
```
###
export let nor
  : | Boolean Boolean => Boolean
  = | false false => true
    | _ _ => false

###
```
do `true .xor(true)` .should_be(`false`)
do `false.xor(true)` .should_be(`true`)
do `true .xor(false)`.should_be(`true`)
do `false.xor(false)`.should_be(`false`)
```
###
export let xor
  : | Boolean Boolean => Boolean
  = | false false => false
    | true true => false
    | _ _ => true

###
```
do `true .xnor(true)` .should_be(`true`)
do `false.xnor(true)` .should_be(`false`)
do `true .xnor(false)`.should_be(`false`)
do `false.xnor(false)`.should_be(`true`)
```
###
export let xnor
  : | Boolean Boolean => Boolean
  = | false false => true
    | true true => true
    | _ _ => false


###
For checking if two expressions are deeply equal.
###
export let equals<A>
  : | A A => Boolean
  = @@@
  /*
    MIT License

    Copyright (c) 2017 Evgeny Poberezkin

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following predicates:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

    Refer https://github.com/epoberezkin/fast-deep-equal/blob/master/LICENSE
  */
    function equal(a, b) {
      if (a === b) return true;

      if (a && b && typeof a == 'object' && typeof b == 'object') {
        if (a.constructor !== b.constructor) return false;

        var length, i, keys;
        if (Array.isArray(a)) {
          length = a.length;
          if (length != b.length) return false;
          for (i = length; i-- !== 0;)
            if (!equal(a[i], b[i])) return false;
          return true;
        }

        if (a.constructor === RegExp) return a.source === b.source && a.flags === b.flags;
        if (a.valueOf !== Object.prototype.valueOf) return a.valueOf() === b.valueOf();
        if (a.toString !== Object.prototype.toString) return a.toString() === b.toString();

        keys = Object.keys(a);
        length = keys.length;
        if (length !== Object.keys(b).length) return false;

        for (i = length; i-- !== 0;)
          if (!Object.prototype.hasOwnProperty.call(b, keys[i])) return false;

        for (i = length; i-- !== 0;) {
          var key = keys[i];

          if (!equal(a[key], b[key])) return false;
        }

        return true;
      }

      // true if both NaN, false otherwise
      return a!==a && b!==b;
    };
  @@@

###
Convert any value to JSON.  
```
do `123.to_json`.should_be(`"123"`)
```
###
export let to_json<A>
  : | A => String
  = @@@ (x) => JSON.stringify(x) @@@

###
For asserting that two values are deep equal.  
Upon failure the process will be exited with code 1.  
```
do `["Hello" " " "world"].join`.should_be(`"Hello world"`)
```
###
export let should_be<A>
  : | `A` `A` => Null
  = | a b => 
      let source = [
        a.meta.filename
        ":"
        a.meta.line_start.plus(1).to_string
        ":"
        a.meta.column_start.plus(1).to_string
      ].join

      a.value.equals(b.value).(
        | true => [ source "(ok)" ].join.print 
        | false =>
            let _ = [ 
              source "(ASSERTION FAILED: LEFT does not deeply equals RIGHT)" 
              "\nLEFT  = " a.value.to_json
              "\nRIGHT = " b.value.to_json
            ].join.print
            1.exit
      )

enum Result<A B> = Ok(A) Error(B)
enum Option<A> = Some(A) None

###
Exit a process with a specific code.
###
export let exit
  : | Integer => Null
  = @@@ (code) => process.exit(code) @@@

# String functions

###
Turns a string into uppercase.  
```
do `"Hello world".to_uppercase`.should_be(`"HELLO WORLD"`)
```
###
export let to_uppercase
  : | String => String
  = @@@ x => x.toUpperCase() @@@

###
Check if a string ends with a particular substring.
```
do `"Hello world".ends_with("world")`.should_be(`true`)
do `"Hello world".ends_with("mars")`.should_be(`false`)
```
###
export let ends_with
  : | String String => Boolean
  = @@@ (a, b) => a.endsWith(b) @@@

###
For joining an array of String.  
```
do `["Hello" " " "World"].join`.should_be(`"Hello World"`)
```
###
export let join
  : | [String] => String
  = @@@ (xs) => xs.join("") @@@

###
Concatenate two strings.
```
do `"Hello".concat(" ").concat("world")`.should_be(`"Hello world"`)
```
###
export let concat
  : | String String => String
  = @@@ (x,y)=>x+y @@@


# Integer functions 

###
For converting an Integer to String.
###
export let to_string
  : | Integer => String
  = @@@ x => x.toString() @@@

###
Add two integers.
```
do `123.plus(987)`.should_be(`1110`)
```
###
export let plus
  : | Integer Integer => Integer
  = @@@ (x, y) => x + y @@@

###
Add two floats.
Note that the end result might have lost of precision due to the nature of floating point arithmetic.
```
do `1.23.plus(4.57).round(1)`.should_be(`5.8`)
```
###
export let plus
  : | Float Float => Float
  = @@@ (x, y) => x + y @@@

###
Round a float to the number of decimals specified.
```
do `1.23456.round(2)`.should_be(`1.23`)
do `1.23456.round(0)`.should_be(`1.0`)
do `1.23456789.round(10)`.should_be(`1.23456789`)
```
###
export let round
  : | Float Integer => Float
  = @@@ (number, decimal) => {
    const tens = Math.pow(10, decimal)
    return Math.round((number + Number.EPSILON) * tens) / tens
  }
  @@@


###
Subtracts two integers.
```
do `100.minus(1)`.should_be(`99`)
```
###
export let minus
  : | Integer Integer => Integer
  = @@@ (x, y) => x - y @@@

###
Subtracts two floats.  
Note that the end result might have lost of precision due to the nature of floating point arithmetic.
```
do `1.0.minus(0.1)`.should_be(`0.9`)
```
###
export let minus
  : | Float Float => Float
  = @@@ (x, y) => x - y @@@

###
Multiply two integers.
```
do `9.multiply(8)`.should_be(`72`)
```
###
export let multiply
  : | Integer Integer => Integer
  = @@@ (x, y) => x * y @@@

###
Multiply two floats.
```
do `9.0.multiply(8.0)`.should_be(`72.0`)
```
###
export let multiply
  : | Float Float => Float
  = @@@ (x, y) => x * y @@@

###
Divide two integers where the remainder will be truncated.
```
do `9.divide(3)`.should_be(`3`)
do `9.divide(4)`.should_be(`2`)
```
###
export let divide
  : | Integer Integer => Integer
  = @@@ (a, b) => Math.floor(a / b) @@@

###
Divide two floats.
```
do `2.7.divide(3.0)`.should_be(`0.9`)
```
###
export let divide
  : | Float Float => Float
  = @@@ (a, b) => a / b @@@

###
Compute `base` raised to the `exponent` power.
```
do `9.power(2)`.should_be(`81`)
```
###
export let power
  : | Integer Integer => Integer
  = @@@ Math.pow @@@

###
Compute `base` raised to the `exponent` power.
```
do `9.0.power(2.3).round(3)`.should_be(`156.588`)
```
###
export let power
  : | Float Float => Float
  = @@@ Math.pow @@@

###
Check if a given float is not a number (NaN).
```
do `0.0.is_not_a_number`.should_be(`false`)
```
###
export let is_not_a_number
  : | Float => Boolean
  = @@@ n => Number.isNaN(n) @@@

###
Check if a given integer is not a number (NaN).
```
do `0.modulo(0).is_not_a_number`.should_be(`true`)
```
###
export let is_not_a_number
  : | Integer => Boolean
  = @@@ n => Number.isNaN(n) @@@

###
Get the remainder of a division between two integers.  
```
do `10.modulo(3)`.should_be(`1`)
do `-13.modulo(64)`.should_be(`51`)
do `0.modulo(0).is_not_a_number`.should_be(`true`)
```
###
export let modulo
  : | Integer Integer => Integer
  = @@@ (a, n) => n === 0 ? NaN : ((a % n) + n) % n @@@

###
Check if the first integer is less than the second integer.
```
do `1.less_than(2)`.should_be(`true`)
do `1.less_than(1)`.should_be(`false`)
do `2.less_than(1)`.should_be(`false`)
```
###
export let less_than
  : | Integer Integer => Boolean
  = @@@ (x,y) => x < y @@@

###
Check if the first float is less than the second float.
```
do `1.0.less_than(2.0)`.should_be(`true`)
do `1.0.less_than(1.0)`.should_be(`false`)
do `2.0.less_than(1.0)`.should_be(`false`)
```
###
export let less_than
  : | Float Float => Boolean
  = @@@ (x,y) => x < y @@@


###
Check if the first integer is more than the second integer.
```
do `1.more_than(2)`.should_be(`false`)
do `1.more_than(1)`.should_be(`false`)
do `2.more_than(1)`.should_be(`true`)
```
###
export let more_than
  : | Integer Integer => Boolean
  = @@@ (x,y) => x > y @@@

###
Check if the first float is more than the second float.
```
do `1.0.more_than(2.0)`.should_be(`false`)
do `1.0.more_than(1.0)`.should_be(`false`)
do `2.0.more_than(1.0)`.should_be(`true`)
```
###
export let more_than
  : | Float Float => Boolean
  = @@@ (x,y) => x > y @@@

# Array functions

###
Append an element to an array.
```
do `[].append(1).append(2)`.should_be(`[1 2]`)
```
###
export let append<A>
  : | [A] A => [A]
  = @@@ (xs, x) => {
    let result = Array(xs.length + 1)
    let length = xs.length
    for(var i = 0; i < length; i++) {
      result[i] = xs[i]
    }
    result[length] = x
    return result
  } @@@
###
Get the element of an array at a specified zero-based index.
```
do `[].at(0)`.should_be(`None`)
do `[1 2 3].at(1)`.should_be(`Some(2)`)
```
###
export let at<A>
  : | [A] Integer => Option<A>
  = @@@ (xs, i) => i < xs.length ? {$:"Some",_:xs[i]} : {$:"None",_:null} @@@

###
Concatenate two arrays.
```
do `[1 2 3].concat([4 5 6])`.should_be(`[1 2 3 4 5 6]`)
do `[].concat([])`.should_be(`[]`)
do `[1].concat([])`.should_be(`[1]`)
do `[].concat([1])`.should_be(`[1]`)
```
###
export let concat<A>
  : | [A] [A] => [A]
  = @@@
    (xs, ys) => {
      var result = Array(xs.length + ys.length)
      var xsLength = xs.length
      for(var i = 0; i < xsLength; i++) {
        result[i] = xs[i]
      }
      var ysLength = ys.length
      for(var i = 0; i < ysLength; i++) {
        result[i + xsLength] = ys[i]
      }
      return result
    }
  @@@

###
Drop N elements of an array starting from the left.
Opposite of `take`.
```
do `[1 2 3 4].drop(2)`.should_be(`[3 4]`)
do `[1 2 3 4].drop(0)`.should_be(`[1 2 3 4]`)
do `[1 2 3 4].drop(5)`.should_be(`[]`)
```
###
export let drop<A>
  : | [A] Integer => [A]
  = @@@
    (xs, n) => {
      if(n < 0) {
        return []
      }
      var length = xs.length
      var result = []
      for(var i = n; i < length; i++) {
        result.push(xs[i])
      }
      return result
    }
  @@@

###
Drop elements that satisfy the given predicate from an array starting from the left.
Opposite of `take_while`.
```
do `[1 2 3 1].drop_while(|.less_than(3))`.should_be(`[3 1]`)
do `[1 2 3 1].drop_while(|.less_than(0))`.should_be(`[1 2 3 1]`)
do `[1 2 3 1].drop_while(|.less_than(9))`.should_be(`[]`)
```
###
export let drop_while<A>
  : | [A] | A => Boolean => [A]
  = @@@
    async (xs, p) => {
      var length = xs.length
      for(var i = 0; i < length; i++) {
        var x = xs[i]
        if(await p(x)) {
          continue
        }
        else {
          var result = []
          for(var j = i; j < length; j++) {
            result.push(xs[j])
          }
          return result
        }
      }
      return []
    }
  @@@

###
Check if every element in this array matches the specified predicate.  
This function will stop iterating as soon as an element is found to NOT match the predicate.
```
do `[1 2 3].every(|.more_than(0))`.should_be(`true`)
do `[1 2 3].every(|.more_than(1))`.should_be(`false`)
```
###
export let every<A>
  : | [A] | A => Boolean => Boolean
  = @@@
  async (xs, predicate) => {
    var length = xs.length
    for(var i = 0; i < length; i++) {
      if(!await predicate(xs[i])) {
        return false
      } 
    }
    return true
  }
  @@@


###
Filter out element from an array that does not fulfil the specified predicate.  
```
do `[1 2 3].filter(|.more_than(1))`.should_be(`[2 3]`)
```
###
export let filter<A>
  : | [A] | A => Boolean => [A]
  = @@@
  async (xs, f) => {
    var result = []
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      if(await f(x)) {
        result.push(x)
      } 
    }
    return result
  }
  @@@

###
Apply a transform function to each matching elements in an array.  
This is more performant than chaining `filter` and `map` as only one round of iteration is needed.
```
enum Animal = Bird({wings: Integer}) Fish({fins: Integer})
let animals
  : [Animal]
  = [
    Bird({wings: 2})
    Fish({fins: 2})
    Bird({wings: 4})
  ]
do `animals.filter_map(|Bird({wings}) => Some(wings) |_ => None)`.should_be(`[2 4]`)
```
###
export let filter_map<A B>
  : | [A] | A => Option<B> => [B]
  = @@@
  async (xs, f) => {
    var results = []
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var result = await f(x)
      if(result.$ === 'Some') {
        results.push(result._)
      }
    }
    return results
  }
  @@@

### 
This function is similar to `filter_map` except that the mapping function only 
applies to the first satisfying element.
```
do `[{id: 1 name: "Foo"} {id: 2 name: "Bar"}]
    .find_map(|x => x.id.equals(2).(| true => Some(x.name) | _ => None))`
    .should_be(`Some("Bar")`)
```
###
export let find_map<A B>
  : | [A] | A => Option<B> => Option<B>
  = @@@
  async (xs, f) => {
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var result = await f(x)
      if(result.$ === 'Some') {
        return result
      }
    }
    return {$: "None", _:null}
  }
  @@@


###
Get the first element of an array.
```
do `[].first`.should_be(`None`)
do `[1 2 3].first`.should_be(`Some(1)`)
```
###
export let first<A>
  : | [A] => Option<A>
  = @@@ 
    xs => {
      var x = xs[0]
      return x ? {$: "Some", _:x} : {$: "None", _:null}
    }
  @@@

###
Aggregate an array into different groups based on the specified key.
```
do
  let data = [{name: "Ali" age: 2} {name: "Bob" age: 21} {name: "Cat" age: 10}]
   `data.group(|.age.more_than(18).(| true => "adult" | false => "child"))`
    .should_be(`[
      {key: "child" first: {name: "Ali" age: 2} rest: [{name: "Cat" age: 10}]}
      {key: "adult" first: {name: "Bob" age: 21} rest: []}
    ]`)
```
###
export let group<A>
  : | [A] | A => String => [{key: String first: A rest: [A]}]
  = @@@ async (xs, f) => {
    var length = xs.length
    var result = {}
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var key = await f(x)
      if(result[key]) {
        result[key].$rest.push(x)
      }
      else {
        result[key] = {$key: key, $first: x, $rest: []}
      }
    }
    return Object.values(result)
  }
  @@@

###
Index each element of an array.
```
do `["Hello" "world"].indexed`.should_be(`[{index: 0 value: "Hello"} {index: 1 value: "world"}]`)
```
###
export let indexed<A>
  : | [A] => [{index: Integer value: A}]
  = @@@ (xs) => {
    let length = xs.length
    let result = Array(length)
    for(var i = 0; i < length; i++) {
      result[i] = {$index: i, $value: xs[i]}
    }
    return result
  }
  @@@

###
Get the last element of an array.
```
do `[].last`.should_be(`None`)
do `[1 2 3].last`.should_be(`Some(3)`)
```
###
export let last<A>
  : | [A] => Option<A>
  = @@@ 
    xs => {
      var x = xs[xs.length - 1]
      return x ? {$: "Some", _:x} : {$: "None", _:null}
    }
  @@@

###
Get the length of an array.
```
do `[].length`.should_be(`0`)
do `[1 2 3].length`.should_be(`3`)
```
###
export let length<A>
  : | [A] => Integer
  = @@@ xs => xs.length @@@

###
Map a function to each element of an array.
```
do `[1 2 3].map(|.plus(1))`.should_be(`[2 3 4]`)
```
###
export let map<A B>
  : | [A] | A => B => [B]
  = @@@
  async (xs, f) => {
    var result = Array(xs.length)
    var length = xs.length
    for(var i = 0; i < length; i++) {
      result[i] = await f(xs[i])
    }
    return result
  }
  @@@


###
Partition an array into two parts based on a test.
```
do `[1 2 3 4].partition(|.more_than(2))`.should_be(`{passed: [3 4] failed: [1 2]}`)
```
###
export let partition<A>
  : | [A] | A => Boolean => {passed: [A] failed: [A]}
  = @@@ async (xs, f) => {
    var length = xs.length
    var passed = []
    var failed = []
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      if(await f(x)) {
        passed.push(x)
      }
      else {
        failed.push(x)
      }
    }
    return {$passed: passed, $failed: failed}
  }
  @@@

###
Reduce an array into a particular value. 
This function is also known as `fold`.  
For more info, refer https://en.wikipedia.org/wiki/Fold_%28higher-order_function%29
```
do `[1 2 3].reduce({from: 0 with: plus})`.should_be(`6`)
```
Another ```
do `[
  {name: "John" age: 6}
  {name: "Bob" age: 7}
  {name: "Ali" age: 8}
  {name: "Lee" age: 9}
].reduce({ from: 0 with: | result current => result.plus(current.age) })`
  .should_be(`30`)
```
###
export let reduce<A B>
  : | [A] { from: B with: | B A => B } => B
  = @@@
  async (xs, {$from, $with}) => {
    let result = $from
    let length = xs.length
    for(var i = 0; i < length; i++) {
      result = await $with(result, xs[i])
    }
    return result
  }
  @@@

###
Repeat the given element N times.
```
do `"Hello".repeat(3)`.should_be(`["Hello" "Hello" "Hello"]`)
do `0.repeat(5)`.should_be(`[0 0 0 0 0]`)
do `[].repeat(2)`.should_be(`[[] []]`)
```
###
export let repeat<A>
  : | A Integer => [A]
  = @@@
    (x, n) => {
      var result = Array(n)
      for(var i = 0; i < n; i++) {
        result[i] = x
      }
      return result
    }
  @@@


###
Check if some element in this array matches the specified predicate.
This function will stop iterating as soon as an element is found to match the predicate.
```
do `[1 2 3].some(|.more_than(1))`.should_be(`true`)
do `[1 2 3].some(|.more_than(3))`.should_be(`false`)
```
###
export let some<A>
  : | [A] | A => Boolean => Boolean
  = @@@
  async (xs, predicate) => {
    var length = xs.length
    for(var i = 0; i < length; i++) {
      if(await predicate(xs[i])) {
        return true
      } 
    }
    return false
  }
  @@@

###
Take N elements of an array, starting from the left.
```
do `[1 2 3].take(2)`.should_be(`[1 2]`)
do `[1 2 3].take(9)`.should_be(`[1 2 3]`)
do `[].take(-1)`.should_be(`[]`)
```
###
export let take<A>
  : | [A] Integer => [A]
  = @@@
    (xs, n) => {
      if(n > 0) {
        var length = Math.min(xs.length, n)
        var result = Array(length)
        for(var i = 0; i < length; i++) {
          result[i] = xs[i]
        }
        return result
      }
      else {
        return []
      }
    }
  @@@

###
Take elements that satisfied the given predicate of an array starting from the left.  
Opposite of `drop_while`.
```
do `[1 2 3 1].take_while(|.less_than(3))`.should_be(`[1 2]`)
do `[1 2 3 1].take_while(|.more_than(4))`.should_be(`[]`)
```
###
export let take_while<A>
  : | [A] | A => Boolean => [A]
  = @@@ 
    async (xs, p) => {
      var result = []
      var length = xs.length
      for(var i = 0; i < length; i++) {
        var x = xs[i]
        if(await p(x)) {
          result.push(x)
        }
        else {
          return result
        }
      }
      return result
    }
  @@@


###
Create a range of integers.  
```
do `1.to(10)`.should_be(`[ 1 2 3 4 5 6 7 8 9 10 ]`)
```
###
export let to
  : | Integer Integer => [Integer]
  = @@@
    (start, end) => {
      let length = end - start + 1
      let result = Array(end - start + 1)
      for(var i = 0; i < length; i++) {
        result[i] = i + start
      }
      return result
    }
  @@@

###
Remove elements that are defined as duplicated.  
```
do `[{id: 1} {id: 2} {id: 3} {id: 1}].unique(|.id.to_string)`
    .should_be(`[{id: 1} {id: 2} {id: 3}]`)
```
###
export let unique<A>
  : | [A] | A => String => [A]
  = @@@
  async (xs, getKey) => {
    var result = {}
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var key = await getKey(x)
      if(!result[key]) {
        result[key] = x
      }
    }
    return Object.values(result)
  }
  @@@

###
Updates each element of an array based on the specified predicate.  
```
do `["apple" "banana" "cane"].update_each({ if: |.ends_with("e") then: |.to_uppercase })`
  .should_be(`["APPLE" "banana" "CANE"]`)
```
###
export let update_each<A>
  : | [A] {if: | A => Boolean then: | A => A} => [A]
  = @@@
  async (xs, {$if, $then}) => {
    var result = []
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      if(await $if(x)) {
        result.push(await $then(x))
      } else {
        result.push(x)
      }
    }
    return result
  }
  @@@



# Time-related functions
enum Duration = Duration({milliseconds: Float})

###
Create a duration given the number of milliseconds. 
```
do `100.0.milliseconds.milliseconds`.should_be(`100.0`)
```
###
export let milliseconds
  : | Float => Duration
  = | n => Duration({milliseconds: n})

###
Create a duration given the number of seconds.
```
do `5.0.seconds.milliseconds`.should_be(`5000.0`)
```
###
export let seconds
  : | Float => Duration
  = | n => n.multiply(1000.0).milliseconds

###
Create a duration given the number of minutes.
```
do `1.0.minutes.milliseconds`.should_be(`60000.0`)
```
###
export let minutes
  : | Float => Duration
  = | n => n.multiply(60.0).seconds

###
Create a duration given the number of hours.
```
do `1.0.hours.milliseconds`.should_be(`3600000.0`)
```
###
export let hours
  : | Float => Duration
  = | n => n.multiply(60.0).minutes

###
Create a duration given the number of hours.
```
do `1.0.days.milliseconds`.should_be(`86400000.0`)
```
###
export let days
  : | Float => Duration
  = | n => n.multiply(24.0).hours

###
Get the number of milliseconds that this duration represents.
```
do `Duration({milliseconds: 1000.0}).milliseconds`.should_be(`1000.0`)
```
###
export let milliseconds
  : | Duration => Float
  = | Duration({milliseconds}) => milliseconds

###
Wait for a given duration.  
```
do 0.01.seconds.wait
```
###
export let wait
  : | Duration => Null
  = @@@ (duration) => 
    new Promise(resolve => setTimeout(resolve, duration._.$milliseconds))
  @@@

###
Benchmark a function.
###
export let benchmark<A>
  : | Null | Null => A => Duration
  = @@@
  async (_, f) => {
    var justNow = Date.now()
    await f()
    return {$: "Duration", _: {$milliseconds: Date.now() - justNow}}
  }
  @@@

export let native_map<A B>
  : | [A] | A => B => [B]
  = @@@ (xs, f) => xs.map(f) @@@

export let native_reduce<A B>
  : | [A] {from: B with: | B A => B} => [B]
  = @@@ (xs, {$from, $with}) => xs.reduce($with, $from) @@@

# do null.benchmark(|_ => 0.to(100000).native_map(|.plus(2))).print
# do null.benchmark(|_ => 0.to(100000).map(|.plus(2))).print

# do null.benchmark(|_ => 0.to(100000).native_reduce({from: 0 with: plus})).print
# do null.benchmark(|_ => 0.to(100000).reduce({from: 0 with: plus})).print