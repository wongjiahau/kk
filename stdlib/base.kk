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

# Option's function
enum Option<A> = Some(A) None

let is_some<A>
  : | Option<A> => Boolean
  = | Some(_) => true
    | _ => false

let is_none<A>
  : | Option<A> => Boolean
  = | None => true
    | _ => false

###
Exit a process with a specific code.
###
export let exit
  : | Int => Null
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


# Int and Float functions 

###
For converting an Int to String.
###
export let to_string
  : | Int => String
  = @@@ x => x.toString() @@@

###
Add two Ints.
```
do `123.plus(987)`.should_be(`1110`)
```
###
export let plus
  : | Int Int => Int
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
  : | Float Int => Float
  = @@@ (number, decimal) => {
    const tens = Math.pow(10, decimal)
    return Math.round((number + Number.EPSILON) * tens) / tens
  }
  @@@


###
Subtracts two Ints.
```
do `100.minus(1)`.should_be(`99`)
```
###
export let minus
  : | Int Int => Int
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
Multiply two Ints.
```
do `9.multiply(8)`.should_be(`72`)
```
###
export let multiply
  : | Int Int => Int
  = @@@ (x, y) => x*y @@@

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
Divide two Ints where the remainder will be truncated.
```
do `9.divide(3)`.should_be(`3`)
do `9.divide(4)`.should_be(`2`)
```
###
export let divide
  : | Int Int => Int
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
  : | Int Int => Int
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
Check if a given Int is not a number (NaN).
```
do `0.modulo(0).is_not_a_number`.should_be(`true`)
```
###
export let is_not_a_number
  : | Int => Boolean
  = @@@ n => Number.isNaN(n) @@@

###
Get the remainder of a division between two Ints.  
```
do `10.modulo(3)`.should_be(`1`)
do `-13.modulo(64)`.should_be(`51`)
do `0.modulo(0).is_not_a_number`.should_be(`true`)
```
###
export let modulo
  : | Int Int => Int
  = @@@ (a, n) => n === 0 ? NaN : ((a % n) + n) % n @@@

###
Check if the first Int is less than the second Int.
```
do `1.less_than(2)`.should_be(`true`)
do `1.less_than(1)`.should_be(`false`)
do `2.less_than(1)`.should_be(`false`)
```
###
export let less_than
  : | Int Int => Boolean
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
Check if the first Int is more than the second Int.
```
do `1.more_than(2)`.should_be(`false`)
do `1.more_than(1)`.should_be(`false`)
do `2.more_than(1)`.should_be(`true`)
```
###
export let more_than
  : | Int Int => Boolean
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

###
Negates a float value.
```
do `1.0.negative`.should_be(`-1.0`)
```
###
export let negative
  : | Float => Float
  = @@@ x => -x @@@

###
Represents positive infinity.
###
export let INFINITY
  : Float
  = @@@ Number.INFINITY @@@

###
Returns the value of pi.
```
do `PI.round(9)`.should_be(`3.141592654`)
```
###
export let PI
  : Float
  = @@@ Math.PI @@@ 

###
This value represents Euler's number, the base of natural logarithms, e.
```
do `E.round(9)`.should_be(`2.718281828`)
```
###
export let E
  : Float
  = @@@ Math.E @@@

###
This value represents the natural logarithm of 10.
```
do `LN_10.round(4)`.should_be(`2.3026`)
```
###
export let LN_10
  : Float
  = @@@ Math.LN10 @@@

###
This value represents the natural logarithm of 2.
```
do `LN_2.round(4)`.should_be(`0.6931`)
```
###
export let LN_2
  : Float
  = @@@ Math.LN2 @@@

###
This value represents the base 10 logarithm of e.
```
do `LOG_10_E.round(4)`.should_be(`0.4343`)
```
###
export let LOG_10_E
  : Float
  = @@@ Math.LOG10E @@@
  
###
This value represents the base 2 logarithm of e.
```
do `LOG_2_E.round(4)`.should_be(`1.4427`)
```
###
export let LOG_2_E
  : Float
  = @@@ Math.LOG2E @@@

###
This value represents the square root of 1/2.
```
do `SQUARE_ROOT_HALF.round(4)`.should_be(`0.7071`)
```
###
export let SQUARE_ROOT_HALF
  : Float
  = @@@ Math.SQRT1_2 @@@

###
This value represents the square root of 2.
```
do `SQUARE_ROOT_2.round(4)`.should_be(`1.4142`)
```
###
export let SQUARE_ROOT_2
  : Float
  = @@@ Math.SQRT2 @@@

###
Returns the absolute value of a number.
```
do `-2.0.absolute`.should_be(`2.0`)
do `1.0.absolute`.should_be(`1.0`)
```
###
export let absolute
  : | Float => Float
  = @@@ Math.abs @@@

###
Compute the arc-cosine of a number.
```
do `0.5.acos.round(4)`.should_be(`1.0472`)
```
###
export let acos
  : | Float => Float
  = @@@ Math.acos @@@

###
Compute the hyperbolic arc-cosine of a number.
```
do `2.0.acosh.round(4)`.should_be(`1.317`)
```
###
export let acosh
  : | Float => Float
  = @@@ Math.acosh @@@

###
Compute the arc-sine of a number.
```
do `0.5.asin.round(4)`.should_be(`0.5236`)
```
###
export let asin
  : | Float => Float
  = @@@ Math.asin @@@

###
Compute the hyperbolic arc-sine of a number.
```
do `1.0.asinh.round(4)`.should_be(`0.8814`)
```
###
export let asinh
  : | Float => Float
  = @@@ Math.asinh @@@

###
Compute the arc-tangent of a number
```
do `1.0.atan.round(4)`.should_be(`0.7854`)
```
###
export let atan
  : | Float => Float
  = @@@ Math.atan @@@

###
Compute the angle in the plane (in radians) between the positive x-axis and the ray from (0,0) to the point (x,y).
```
do `1.0.atan2(3.0).round(4)`.should_be(`0.3218`)
```
###
export let atan2
  : | Float Float => Float
  = @@@ Math.atan2 @@@

###
Compute the hyperbolic arc-tangent of a number.
```
do `0.5.atanh.round(4)`.should_be(`0.5493`)
```
###
export let atanh
  : | Float => Float
  = @@@ Math.atanh @@@

###
Compute the cube root of a number.
```
do `27.0.cube_root`.should_be(`3.0`)
```
###
export let cube_root
  : | Float => Float
  = @@@ Math.cbrt @@@

###
Round up a float to the nearest Int.
```
do `1.0001.ceiling`.should_be(`2`)
```
###
export let ceiling
  : | Float => Int
  = @@@ Math.ceil @@@

###
Compute the cosine of an angle in radian.
```
do `PI.cos`.should_be(`-1.0`)
```
###
export let cos
  : | Float => Float
  = @@@ Math.cos @@@

###
Compute the hyperbolic cosine of a number.
```
do `1.0.cosh.round(4)`.should_be(`1.5431`)
```
###
export let cosh
  : | Float => Float
  = @@@ Math.cosh @@@

###
Compute the natural exponentiation of a number.
```
do `E.power(3.0).round(9)`.should_be(`3.0.exp.round(9)`)
```
###
export let exp
  : | Float => Float
  = @@@ Math.exp @@@

###
Compute the natural exponentiation of a number, minus 1.
```
do `E.power(3.0).minus(1.0).round(9)`.should_be(`3.0.expm1.round(9)`)
```
###
export let expm1
  : | Float => Float
  = @@@ Math.expm1 @@@

###
Round down a float to the nearest Int.
```
do `9.9999.floor`.should_be(`9`)
```
###
export let floor
  : | Float => Int
  = @@@ Math.floor @@@

###
Compute the natural logarithm of a number.
```
do `E.log`.should_be(`1.0`)
```
###
export let log
  : | Float => Float
  = @@@ Math.log @@@

###
Compute the base 10 logarithm of a number.
```
do `10.0.log10`.should_be(`1.0`)
```
###
export let log10
  : | Float => Float
  = @@@ Math.log10 @@@

###
Compute the base 2 logarithm of a number.
```
do `2.0.log2`.should_be(`1.0`)
```
###
export let log2
  : | Float => Float
  = @@@ Math.log2 @@@

###
Returns the maximum float of array.
```
do `[1.2 52.0 3.3 2.3].max`.should_be(`52.0`)
```
###
export let max<A>
  : | [Float] => Float
  = @@@ (xs) => Math.max(...xs) @@@

###
Returns the minimum float of array.
```
do `[1.2 52.0 3.3 2.3].min`.should_be(`1.2`)
```
###
export let min<A>
  : | [Float] => Float
  = @@@ (xs) => Math.min(...xs) @@@

###
Compute the sine of a number.
```
do `0.0.sin`.should_be(`0.0`)
```
###
export let sin
  : | Float => Float
  = @@@ Math.sin @@@


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
Get the element of an array get a specified zero-based index.
```
do `[].get(0)`.should_be(`None`)
do `[1 2 3].get(1)`.should_be(`Some(2)`)
```
###
export let get<A>
  : | [A] Int => Option<A>
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
  : | [A] Int => [A]
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
     (xs, p) => {
      var length = xs.length
      for(var i = 0; i < length; i++) {
        var x = xs[i]
        if( p(x)) {
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
  = | xs predicate => xs
      .find_map(|x => x.predicate.(|false => Some(null) |_ => None))
      .is_some.not

###
Filter out element from an array that does not satisfy the given predicate.  
```
do `[1 2 3].filter(|.more_than(1))`.should_be(`[2 3]`)
```
###
export let filter<A>
  : | [A] | A => Boolean => [A]
  = | xs predicate => xs.flat_map(|x => x.predicate.(|true => [x] |_ => []))

###
Apply a transform function to each matching elements in an array.  
This is more performant than chaining `filter` and `map` as only one round of iteration is needed.
```
enum Animal = Bird({wings: Int}) Fish({fins: Int})
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
  = | xs f => xs.flat_map(|x => x.f.(|Some(y) => [y] |_ => []))

###
Find the first element that satisfies the given predicate.
```
do `[1 2 3].find(|.more_than(2))`.should_be(`Some(3)`)
do `[1 2 3].find(|.more_than(10))`.should_be(`None`)
```
###
export let find<A>
  : | [A] | A => Boolean => Option<A>
  = | xs predicate => xs.find_map(|x => x.predicate.(|true => Some(x) |_ => None))


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
  = 
  @@@
   (xs, f) => {
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var result =  f(x)
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
Flatten an array.
```
do `[[1] [2 3] [4]].flat`.should_be(`[1 2 3 4]`)
```
###
export let flat<A>
  : | [[A]] => [A]
  = @@@ xs => xs.flat() @@@


###
Apply a function to each element of an array an flatten the result.
```
do `[1 2 3].flat_map(|x => [x x])`.should_be(`[1 1 2 2 3 3]`)
```
###
export let flat_map<A B>
  : | [A] | A => [B] => [B]
  = @@@
     (xs, f) => {
      var result = []
      var length = xs.length
      for(var i = 0; i < length; i++) {
        result.push(... f(xs[i]))
      }
      return result
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
  = @@@  (xs, f) => {
    var length = xs.length
    var result = {}
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var key =  f(x)
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
  : | [A] => [{index: Int value: A}]
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
  : | [A] => Int
  = @@@ xs => xs.length @@@


enum Continuation<State Answer> = Continue(State) Break(Answer)

###
Loop with a state.
For example, the factorial function can be implemented as follows:
```
let factorial
  : | Int => Int
  = | n => {n result: 1}.loop(|{n result} =>
      n.less_than(2).(
        | true => Break(result)
        | _ => Continue({n: n.minus(1) result: result.multiply(n) })
      )
    )

do `0.factorial`.should_be(`1`)
do `10.factorial`.should_be(`3628800`)
```
###
export let loop<A B>
  : | A | A => Continuation<A B> => B
  = @@@ (state, f) => {
    while(true) {
      let next = f(state)
      if(next.$==='Continue') {
        state = next._
      }
      else {
        return next._
      }
    }
  }
  @@@

let naive_map_2<A B>
  : | [A] | A => B => [B]
  = | xs f => {index: 0 result: []}.loop(|state =>
      xs.get(state.index).(
        | Some(x) => Continue(state.{index.plus(1) result.append(x.f)})
        | None => Break(state.result)
      )
    )


###
Map a function to each element of an array.
```
do `[1 2 3].map(|.plus(1))`.should_be(`[2 3 4]`)
```
###
export let map<A B>
  : | [A] | A => B => [B]
  = | xs f => xs.flat_map(| x => [x.f])

###
Partition an array into two parts based on a test.
```
do `[1 2 3 4].partition(|.more_than(2))`.should_be(`{passed: [3 4] failed: [1 2]}`)
```
###
export let partition<A>
  : | [A] | A => Boolean => {passed: [A] failed: [A]}
  = @@@  (xs, f) => {
    var length = xs.length
    var passed = []
    var failed = []
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      if( f(x)) {
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
   (xs, {$from, $with}) => {
    let result = $from
    let length = xs.length
    for(var i = 0; i < length; i++) {
      result =  $with(result, xs[i])
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
  : | A Int => [A]
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
Reverse an array. The original array will not be mutated.
```
do `[1 2 3].reverse`.should_be(`[3 2 1]`)
do `[].reverse`.should_be(`[]`)
```
###
export let reverse<A>
  : | [A] => [A]
  = @@@
    (xs) => {
      var length = xs.length
      var result = Array(length)
      for(var i = 0; i < length; i++) {
        result[i] = xs[length - i -1]
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
  = | xs predicate => xs.find_map(|x => x.predicate.(|true => Some(null) |_ => None)).is_some

enum Order = Lesser Equals Greater
###
Sort an array based on a compare function.
```
#do `[1 3 4 2].sort_by(
#      | a b if a.more_than(b) => Greater 
#      | a b if a.less_than(b) => Lesser 
#      | _ _                   => Equals
#    )`.should_be(`[1 2 3 4]`)
```
###
export let sort_by<A>
  : | [A] | A A => Order => [A]
  = @@@ 
     (xs, f) => {
      throw new Error("Not implemented yet")
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
  : | [A] Int => [A]
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
     (xs, p) => {
      var result = []
      var length = xs.length
      for(var i = 0; i < length; i++) {
        var x = xs[i]
        if( p(x)) {
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
Create a range of Ints.  
```
do `1.to(10)`.should_be(`[ 1 2 3 4 5 6 7 8 9 10 ]`)
```
###
export let to
  : | Int Int => [Int]
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
   (xs, getKey) => {
    var result = {}
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      var key =  getKey(x)
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
   (xs, {$if, $then}) => {
    var result = []
    var length = xs.length
    for(var i = 0; i < length; i++) {
      var x = xs[i]
      if( $if(x)) {
        result.push( $then(x))
      } else {
        result.push(x)
      }
    }
    return result
  }
  @@@

###
Zip two arrays together.
```
do `[1 2].zip(["a" "b"])`.should_be(`[{left: 1 right: "a"} {left: 2 right: "b"}]`)
do `[1 2 3].zip(["a"])`.should_be(`[{left: 1 right: "a"}]`)
```
###
export let zip<A B>
  : | [A] [B] => [{left: A right: B}]
  = @@@
    (xs, ys) => {
      var length = Math.min(xs.length, ys.length)
      var result = Array(length)
      for(var i = 0; i < length; i++) {
        result[i] = {$left: xs[i], $right: ys[i]}
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
    console.log( Date.now() - justNow, "ms")
    return {$: "Duration", _: {$milliseconds: Date.now() - justNow}}
  }
  @@@

enum File = File({path: String})

# File system functions
export let read_file
  : | File => !Result<String String>
  = @@@
    ({_: {$path}}) => {
      return new Promise((resolve, reject) => {
        require("fs").readFile($path, 'utf8', (error, result) => {
          if(error) {
            reject(error)
          }
          else {
            resolve(result.toString())
          }
        })
      })
    }
  @@@


# Garbage code for testing

export let native_map<A B>
  : | [A] | A => B => [B]
  = @@@ (xs, f) => xs.map(f) @@@

export let native_reduce<A B>
  : | [A] {from: B with: | B A => B} => [B]
  = @@@ (xs, {$from, $with}) => xs.reduce($with, $from) @@@

let native_push<A>
  : | [A] A => [A]
  = @@@ (xs, x) => {
      xs.push(x)
      return xs
    } @@@

# do 0.to(10).native_map(|.plus(2)).print

let naive_map<A B>
  : | [A] | A => B => [B]
  = | [] _ => []
    | [x...xs] f => [x.f].concat(xs.naive_map(f))


# do null.benchmark(|_ => 0.to(100000).naive_map_2(|.plus(2))).print
# do null.benchmark(|_ => 0.to(1000000).native_map(|.plus(2))).print
# do null.benchmark(|_ => 0.to(1000000).map(|.plus(2))).print
# do null.benchmark(|_ => 0.to(100000).reduce({from: [] with: |result x => result.native_push(x.plus(2))})).print
# do null.benchmark(|_ => [1].repeat(100000).flat).print
# do null.benchmark(|_ => 0.to(100000).filter_map(|n => n.modulo(2).equals(0).(|true => Some(n.multiply(2)) | _ => None))).print

# do null.benchmark(|_ => 0.to(100000).native_reduce({from: 0 with: plus})).print
# do null.benchmark(|_ => 0.to(100000).reduce({from: 0 with: plus})).print
# do null.benchmark(|_ => 0.to(100000).update_each({if: |.modulo(2).equals(0) then: |.plus(2)})).print
# do null.benchmark(|_ => 0.to(10000000).find(|.less_than(-1))).print
# do null.benchmark(|_ => 100.factorial).print

do 20.factorial.print

let x: !Result<Null String>
  = let! Ok(file) = File({path: "./stdlib/base.kk"}).read_file
      else | Error(error) => Error(error)
    let _ = file.print
    !Ok(null)