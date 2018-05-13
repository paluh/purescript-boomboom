module Test.BoomBoom.Generic.Interpret where

import Prelude hiding (unit)

import BoomBoom (parse, serialize)
import BoomBoom.Generic.Interpret (class Interpret, B(B), R(R), Root, V(V), interpret)
import BoomBoom.Strings (int, string, unit)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import Prelude as Prelude
import Test.Unit (TestSuite, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))

genBuilder ∷ forall a b. Interpret "builder" Root a b ⇒ a → b
genBuilder = interpret (SProxy ∷ SProxy "builder")

genBoomboom ∷ forall a b. Interpret "boomboom" Root a b ⇒ a → b
genBoomboom = interpret (SProxy ∷ SProxy "boomboom")

-- | Not really systematicall inference test cases
vvb =  V { a : V { b : B int }}
vvb' = genBuilder vvb

vvbb =  V { a : V { b : B int, c: B int }}
vvbb' = genBuilder vvbb

vvvbb =  V { s: V { a : V { b : B int, c: B int }}}
vvvbb' = genBuilder vvvbb

vvrb =  V { v: V { r : R { b : B int }}}
vvrb' = genBuilder vvrb

vbvb =  V { a : B unit, c: V { d : B unit }, d: B unit}
vbvb' = genBuilder vbvb

vrb =  V { a : R { b : B int }}
vrb' = genBuilder vrb

vrbb =  V { a : R { b : B int }, c: B int}
vrbb' = genBuilder vrb

rrb =  R { a : R { b : B int }}
rrb' = genBuilder rrb

rrbb =  R { a : R { b : B int }, b: B int}
rrbb' = genBuilder rrbb

rrrbb =  R { s: R { a : R { b : B int }, b: B int }}
rrrbb' = genBuilder rrbb

suite ∷ ∀ e. TestSuite e
suite = do
  Test.Unit.suite "BoomBoom.Generic.Interpret" $ do
    Test.Unit.suite "nested variants" $ do
      let
        r = V { c : V { d: B int, e: B int, f: V { g: B int } }}
        builder = genBuilder r
        boomboom = genBoomboom r
      test "serializes correctly" $ do
        equal ("c":"d":"8":Nil) (serialize boomboom (builder.c.d 8))
        equal ("c":"e":"9":Nil) (serialize boomboom (builder.c.e 9))
        equal ("c":"f":"g":"10":Nil) (serialize boomboom (builder.c.f.g 10))

        equal
          (parse boomboom ("c":"d":"8":Nil))
          (Just $ builder.c.d 8)
    Test.Unit.suite "record with variants" $ do
      let
        r = R { a: V { x: B int, y: R { s: B int, t: B int }}, b: B int}
        builder = genBuilder r
        boomboom = genBoomboom r
      test "serializes correctly" $ do
        -- | Records are turned into functions. When you are providing argument
        -- | to this builder you should replace all attributes which are
        -- | variants with function. Builder will pass this variant builder
        -- | to your function so you can create final variant easily.
        equal ("x":"8":"30":Nil) (serialize boomboom (builder {a: \b → b.x 8, b: 30}))
        equal ("y":"8":"9":"30":Nil) (serialize boomboom (builder {a: \b → b.y { s: 8, t: 9}, b: 30}))

        equal (unsafeStringify $ parse boomboom ("x":"8":"30":Nil)) (unsafeStringify $ Just $ builder {a: \b → b.x 8, b: 30})
        equal (unsafeStringify $ parse boomboom ("y":"8":"9":"30":Nil)) (unsafeStringify $ Just $  (builder {a: \b → b.y { s: 8, t: 9}, b: 30}))

    Test.Unit.suite "variant with empty label" $ do
      let
        r = V
          { product: B string
          , login: B unit
          , logout: B unit
          , "": B unit
          }
        builder = genBuilder r
        boomboom = genBoomboom r
      test "serializes correctly" $ do
        equal ("":Nil) (serialize boomboom (builder."" Prelude.unit))
        equal ("product":"89asdf":Nil) (serialize boomboom (builder.product "89asdf"))
      test "parses correctly" $ do
        equal (parse boomboom ("":Nil)) (Just $ builder."" Prelude.unit)
        equal (parse boomboom ("product":"89asdf":Nil)) (Just $ builder."product" "89asdf")
