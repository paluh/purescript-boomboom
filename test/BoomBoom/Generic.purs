module Test.BoomBoom.Generic where

import Prelude

import BoomBoom (exhaustive, parse, serialize, xrap)
import BoomBoom.Generic (record, variant)
import BoomBoom.String (_lit, int)
import BoomBoom.String as BBString
import Control.Monad.Free (Free)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj)
import Global.Unsafe (unsafeStringify)
import Test.Unit (TestF, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))


newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
derive instance newtypeR ∷ Newtype R _
instance showR ∷ Show R where
  show = unsafeStringify

wrapR = xrap :: BBString.BoomBoomR ( x ∷ Int, y ∷ Int, z ∷ Int ) -> BBString.BoomBoom R

suite :: forall eff. Free (TestF eff) Unit
suite = do
  Test.Unit.suite "BoomBoom.Generic" $ do
    let
      recordB :: BBString.BoomBoomR ( x :: Int, y :: Int, z :: Int )
      recordB = record { x: int, y: int, z: int }
    Test.Unit.suite "simple record boomboom" $ do
      test "serializes correctly" $ do
        equal ("1":"2":"3":Nil) (serialize recordB { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ("1":"2":"3":Nil))
        equal Nothing (R <$> parse recordB ("1":"2":"3":"4":Nil))

    let
      variantB :: BBString.BoomBoomV ( zero :: Unit, one :: Int, two :: R )
      variantB = exhaustive $ variant _lit
        { zero: mempty :: BBString.BoomBoom Unit
        , one: int
        , two: wrapR recordB
        }
    Test.Unit.suite "simple variant boomboom" $ do
      let
        wrong = fromFoldable ["wrong", "8"]
        zi = fromFoldable ["zero"]
        oi = fromFoldable ["one", "1"]
        ti = fromFoldable ["two", "2", "3", "4"]
        zv = inj (SProxy ∷ SProxy "zero") unit
        ov = inj (SProxy ∷ SProxy "one") 1
        tv = inj (SProxy ∷ SProxy "two") (R {x: 2, y: 3, z: 4})
      test "serializes correctly" $ do
        equal zi (serialize variantB zv)
        equal oi (serialize variantB ov)
        equal ti (serialize variantB tv)
      test "parses correctly" $ do
        equal Nothing (parse variantB wrong)
        equal Nothing (parse variantB ("1":"one":Nil))
        equal Nothing (parse variantB ("one":"1":"extra":Nil))
        equal (Just zv) (parse variantB zi)
        equal (Just ov) (parse variantB oi)
        equal (Just tv) (parse variantB ti)

    Test.Unit.suite "nested structures" $ do
      let
        nestedB = exhaustive $ variant _lit
          { p1: variant _lit
              { sp1: wrapR (record { x: int, y: int, z: int })
              , sp2: int
              }-- :: BBString.BoomBoomV ( sp1 :: R, sp2 :: Int )
          , p2: int
          } :: BBString.BoomBoomV ( p1 :: Variant ( sp1 :: R, sp2 :: Int ), p2 :: Int )
        i = fromFoldable ["p1", "sp1", "1", "2", "3"]
        iv = inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") (R {x: 1, y: 2, z: 3}))
      test "serializes correctly" $ do
        equal i (serialize nestedB iv)
      test "parses correctly" $ do
        equal Nothing (parse nestedB $ fromFoldable ["p1", "sp1", "1", "2", "3", "extra"])
        equal (Just iv) (parse nestedB i)
