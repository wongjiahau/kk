// no more callback hell
{
  <-: value match {
    n #ok (): n | function,
    err: err,
  } 
    given function 
    given value,

  a + b
    given a <- (1 #ok ())
    given b <- (2 #ok ())
    |.print,

  a + b
    given a <- ("oh no" #error ())
    given b <- (2 #ok ())
    |.print,
}
