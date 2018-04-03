module Test.BoomBoom.Generic where

import Prelude

import BoomBoom (BoomBoom(BoomBoom), parse, serialize, xrap, (>-))
import BoomBoom.Generic (record)
import BoomBoom.Strings (int, variant)
import BoomBoom.Strings as BoomBoom.Strings
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (inj)
import Global.Unsafe (unsafeStringify)
import Test.Unit (TestSuite, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(SProxy))

newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
derive instance newtypeR ∷ Newtype R _
instance showR ∷ Show R where
  show = unsafeStringify

suite :: forall e. TestSuite e
suite = do
  Test.Unit.suite "BoomBoom.Generic" $ do
    let
      recordB = record { x: int, y: int, z: int }
    Test.Unit.suite "simple record boomboom" $ do
      test "serializes correctly" $ do
        equal ("1":"2":"3":Nil) (serialize recordB { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ("1":"2":"3":Nil))

    let
      variantB = variant
        { zero: BoomBoom $ pure unit
        , one: int
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

    Test.Unit.suite "nested structures" $ do
      let
        nestedB = variant
          { p1: variant
              { sp1: ((xrap $ record { x: int, y: int, z: int }) ∷ BoomBoom.Strings.BoomBoom R)
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
