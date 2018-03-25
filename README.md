# purescript-boomboom

because you can boom in both directions!

## Description

Bidirectional routing library with principled plumbing which provides easy to use generic sugar for variants and records.

Pre α stage... docs too.

## `BoomBoom`

The core type of this library is `BoomBoom.Prim.BoomBoom` which translates really to this simple record:

```purescript
newtype BoomBoom tok a = BoomBoom { prs: tok → Maybe a, ser: a → tok }
```

So our `BoomBoom tok a` is a simple parser from `tok` to `a` and also a total serializer function in oposite direction.

## Usage

### Generic helpers

These are tests fragments (`test/BoomBoom/Generic.purs`) for basic session which use generic helpers:

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
    equal ["1", "2", "3"] (serialize recordB { x: 1, y: 2, z: 3 })

  test "parses correctly" $ do
    equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ["1", "2", "3"])
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
    wrong = ["wrong", "8"]
    zi = ["zero"]
    oi = ["one", "1"]
    ti = ["two", "2", "3", "4"]
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
  i = ["p1", "sp1", "1", "2", "3"]
  iv = inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") (R {x: 1, y: 2, z: 3}))
test "serializes correctly" $ do
  equal i (serialize nestedB iv)
test "parses correctly" $ do
  equal (Just iv) (parse nestedB i)
```

### `Applicative` API

And here is completely different approach which uses `apply` and `BoomBoom.Prim.diverge` (aka `(>-)`):


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
300test800
```

### TODO

I'm still working on convinient API for serialization because:

```purescript
  serialize $ inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") (R {x: 1, y: 2, z: 3}))
```

is not a nice API :-)

