module Test.BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom (parse, serialize)
import BoomBoom.Generic.Interpret (B(..), InterpretProxy(..), R(..), Root, V(..), interpret)
import BoomBoom.Strings (int)
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import Test.Unit (suite) as Test.Unit
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))

genBuilder = interpret (SProxy ∷ SProxy "builder")
genBoomboom = interpret (SProxy ∷ SProxy "boomboom")

suite = do
  Test.Unit.suite "BoomBoom.Generic.Interpret" $ do
    Test.Unit.suite "nested variants" $ do
      let
        r = V { c : V { d: B int, e: B int, f: V { g: B int } }}
        builder = genBuilder r
        boomboom = genBoomboom r
      test "serializes correctly" $ do
        equal ["c", "d", "8"] (serialize boomboom (builder.c.d 8))
        equal ["c", "e", "9"] (serialize boomboom (builder.c.e 9))
        equal ["c", "f", "g", "10"] (serialize boomboom (builder.c.f.g 10))

        equal
          (parse boomboom ["c", "d", "8"])
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
        equal ["x", "8", "30"] (serialize boomboom (builder {a: \b → b.x 8, b: 30}))
        equal ["y", "8", "9", "30"] (serialize boomboom (builder {a: \b → b.y { s: 8, t: 9}, b: 30}))

        equal (unsafeStringify $ parse boomboom ["x", "8", "30"]) (unsafeStringify $ Just $ builder {a: \b → b.x 8, b: 30})
        equal (unsafeStringify $ parse boomboom ["y", "8", "9", "30"]) (unsafeStringify $ Just $  (builder {a: \b → b.y { s: 8, t: 9}, b: 30}))


