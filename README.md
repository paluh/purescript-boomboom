# purescript-boomboom

Never hard code your urls again. Boomboom them all!

## Description

Bidirectional routing library with principled plumbing which provides easy to use generic sugar for variants and records.

Still β stage...

## `BoomBoom`

The core type of this library is `BoomBoom.BoomBoom` which translates really to this simple record:

```purescript
newtype BoomBoom tok a = BoomBoom { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }, ser ∷ a → tok }
```

So our `BoomBoom tok a` is a simple parser from `tok` to `a` and also a total serializer function in opposite direction. Composability of this type requires usually that `tok` has a `Semigroup` instance. For details check `BoomBoom.BoomBoom` module docs.

## Usage

### Basic generic helpers

Please note that more user friendly API is described in the next section.

These are tests fragments (`test/BoomBoom/Generic.purs`) which use basic generic helpers:

* `tok` in this case is an `Array String`

* `int` is `BoomBoom (Array String) Int`

* `record` is a helper which builds `BoomBoom` for a give record of `BoomBooms`

* `R` is a record wrapper which provides `Eq` ;-)

Record `BoomBoom` generation and usage:

```purescript
let
  recordB = BoomBoom.Generic.record { x: int, y: int, z: int }
Test.Unit.suite "simple record boomboom" $ do

  test "serializes correctly" $ do
    equal ("1":"2":"3":Nil) (serialize recordB { x: 1, y: 2, z: 3 })

  test "parses correctly" $ do
    equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ("1":"2":"3":Nil))
```

Variant `BoomBoom` generation and usage (with `variant` helper):

```purescript
let
  variantB = variant
    { zero: BoomBoom $ pure unit
    , one: int
    -- | Here we are wrapping our record in `R`
    -- | it is only required because we want
    -- | to perform `eq`...
    , two: BoomBoom $ R <$> unwrap >- recordB
    }
Test.Unit.suite "simple variant boomboom" $ do
  let
    wrong = ("wrong":"8":Nil)
    zi = ("zero":Nil)
    oi = ("one":"1":Nil)
    ti = ("two":"2":"3":"4":Nil)
    zv = inj (SProxy ∷ SProxy "zero") unit
    ov = inj (SProxy ∷ SProxy "one") 1
    tv = inj (SProxy ∷ SProxy "two") (R {x: 2, y: 3, z: 4})
  test "serializes correctly" $ do
    equal zi (serialize variantB zv)
    equal oi (serialize variantB ov)
    equal ti (serialize variantB tv)
  test "parses correctly" $ do
    equal Nothing (parse variantB wrong)
    equal (Just zv) (parse variantB zi)
    equal (Just ov) (parse variantB oi)
    equal (Just tv) (parse variantB ti)
```

Of course you can compose and mix these `BoomBooms` and build arbitrarly large routers/serializers:

```purescript
let
  nestedB = variant
    { p1: variant
        -- | Here again our R which is here just for `eq`
        { sp1: ((xrap $ record { x: int, y: int, z: int }) ∷ BoomBoom _ R)
        , sp2: int
        }
    , p2: int
    }
  i = ("p1":"sp1":"1":"2":"3":Nil)
  iv = inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") (R {x: 1, y: 2, z: 3}))
test "serializes correctly" $ do
  equal i (serialize nestedB iv)
test "parses correctly" $ do
  equal (Just iv) (parse nestedB i)
```

### Typelevel interpretation and helpers

Let's assume that we have routes from previous example and want to produce this serialization `p1/sp1/1/2/3` result. To do this we have to build our variant by hand (as in previous example):

```purescript
  serialize $ inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") {x: 1, y: 2, z: 3})
```

This API is not really readable and easy to use as we have to nest all these `inj` functions with `SProxy` constructors.

In `BoomBoom.Generic.Interpret` you can find "type level interpreters" which are able to produce `BoomBooms` but also easy to use builders of values (variants/records) from "records tree".
Instead of building `BoomBooms` directly you should define your tree using provided constructors:

```purescript
import BoomBoom.Generic.Interpret (V, R, B)
import BoomBoom.Strings (int)

desc = V
  { p1: V
      { sp1: R { x: B int, y: B int, z: B int }
      , sp2: B int
      }
  , p2: B int
  }
```

Now you can produce your `BoomBoom` value but also a `builder`:

```purescript
import BoomBoom.Generic.Interpret (interpret, Root, InterpretProxy)

-- | Generate a builder
builder = interpret (SProxy ∷ SProxy "builder") desc

-- | Generate a BoomBoom
boomboom = interpret (SProxy ∷ SProxy "boomboom") desc

```

This `builder` is a record (for variants build up) or function (for records build up) which helps us build values ready for serialization. So for example this:

```purescript
v = builder.p1.sp1 {x: 1, y: 2, z: 3}
```

Is equivalent of this:

```purescript
v = inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") {x: 1, y: 2, z: 3})
```

Now we can use this value and serialize it:

```purescript
serialize boomboom v
```

Output (in case of `BoomBoom.Strings` serialization) value:

```purescript
("p1":"sp1":"1":"2":"3":Nil)
```

### `Applicative` API

And here is completely different approach which uses `apply` and `BoomBoom.diverge` (aka `(>-)`):


```purescript
path :: BoomBoom String { x :: Int, y :: Int }
path = BoomBoom $
  { x: _, y: _ }
    <$> _.x >- int
    <* lit "test"
    <*> _.y >- int

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ unsafeStringify (parse path ("8080":"test":"200":Nil))

  log (serialize path { x: 300, y: 800 })
```

Output values:

```shell
{"value0":{"x":8080,"y":200}}

-- | I've replaced here `Cons` with (:) to simplify reading
("300":"test":"800":Nil)
```

## TODO

There is ongoing work for "API interpreters" which would generate records clients and server helpers but also docs etc.

## Credits

* A lot of inspirations and crucial suggestions I received from @MonoidMusician. Thanks!

* Initial design of diverging instances was inspired by this @sebastiaanvisser [answer](https://www.reddit.com/r/haskell/comments/38o0f7/a_mixture_of_applicative_and_divisible/#thing_t1_crwh6le).

* Name loosely inspired by this @notcome [gist](https://gist.github.com/notcome/c9d4c750985230d7e346).
