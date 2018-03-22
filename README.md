# purescript-boomboom

because you can boom in both directions!

## Description

It is still just an experiment - for production ready solutions please check:
* [purescript-routing-bob](https://github.com/paluh/purescript-routing-bob)
* [purescript-boomerang](https://github.com/paluh/purescript-boomerang)

I'm trying to implement bidirectional routing with "nearly" applicative interface here (inspired by this [sfvisser answer](https://www.reddit.com/r/haskell/comments/38o0f7/a_mixture_of_applicative_and_divisible/#thing_t1_crwh6le)). What I mean by "nearly" is this:

```purescript
path :: BoomBoom String { x :: Int, y :: Int }
path = BoomBoom $
  { x: _, y: _ }
    <$> (_.x >- int)
    <* lit "test"
    <*> (_.y >- int)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ unsafeStringify (parse path "8080test200")

  log (serialize path { x: 300, y: 800 })
```

Output:

```shell
{"value0":{"x":8080,"y":200}}
300test800
```
