let match<A B>
  : | A | A => B => B
  = | x f => x.f()

let map<A B>
  : | [A] | A => B => [B]
  = | [] _ => []
    | [head ..._] f => [head.f()]

do
  let result = [{foo: 2 bar: "hi"}].map(|.zap)
  null
