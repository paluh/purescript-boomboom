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
    <$> _.x >- int
    <* lit "test"
    <*> _.y >- int

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ unsafeStringify (parse path "8080test200")

  log (serialize path { x: 300, y: 800 })
```

Output:

```shell
{"value0":{"x":8080,"y":200}}
(Just 300test800)
```

I know that serialization result wrapped in `Maybe` can be a bit tedious, but it is required (I've not found any other solution) to implement `Alt`. I hope that generic interface (which I want to provide) will come to the rescue... Or I can drop `Alt` instance and try to provide some helpers like `onB`, `caseB` etc.

Session with `Alt` and `variant` helper:

```purescript
record = BoomBoom $
  {x: _, y: _}
  <$> _.x >- int
  <* lit "/"
  <*> _.y >- int

three' = BoomBoom $
  variant (SProxy ∷ SProxy "one") int
  <|> variant (SProxy ∷ SProxy "two") record
  <|> variant (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)


main = do
  logShow (serialize three' (inj (SProxy ∷ SProxy "two") {x: 8, y: 9}))
  logShow (serialize three' (inj (SProxy ∷ SProxy "zero") unit))
  logShow (serialize three' (inj (SProxy ∷ SProxy "one") 8))

  traceAnyA (parse three' "zero")
  traceAnyA (parse three' "one8")
  traceAnyA (parse three' "two8/9")
```

Output:

```shell
(Just "two8/9")
(Just "zero")
(Just "one8")
Just { value0: { type: 'zero', value: {} } }
Just { value0: { type: 'one', value: 8 } }
Just { value0: { type: 'two', value: { x: 8, y: 9 } } }
```

## Credits

* A lot of inspirations and crucial suggestions I've got from @MonoidMusician. Thanks!

* Initial design of diverging instances was inspired by this @sebastiaanvisser [answer](https://www.reddit.com/r/haskell/comments/38o0f7/a_mixture_of_applicative_and_divisible/#thing_t1_crwh6le)

* Name loosely inspired by this @notcome [gist](https://gist.github.com/notcome/c9d4c750985230d7e346)

