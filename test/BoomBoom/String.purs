module Test.BoomBoom.String where

import Prelude

import BoomBoom (addFieldAfter, addFieldBefore, baseCase, end, exhaustive, parse, serialize, xrap)
import BoomBoom.String (addListPrefix, addPrefixedCase, int)
import BoomBoom.String as BBString
import Control.Monad.Free (Free)
import Data.Functor.Invariant (imap)
import Data.List (List(..), Pattern(..), fromFoldable, singleton, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), inj)
import Global.Unsafe (unsafeStringify)
import Test.Unit (TestF, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)

newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
derive instance newtypeR ∷ Newtype R _
instance showR ∷ Show R where
  show = unsafeStringify

addListPrefixOf :: forall a. String -> BBString.BoomBoom a -> BBString.BoomBoom a
addListPrefixOf = addListPrefix <<< Pattern <<< singleton
wrapR = xrap :: BBString.BoomBoomR ( x ∷ Int, y ∷ Int, z ∷ Int ) -> BBString.BoomBoom R

start = imap (const {}) (const unit) mempty :: BBString.BoomBoomR ()

suite :: forall eff. Free (TestF eff) Unit
suite = do
  Test.Unit.suite "BoomBoom.String" $ do
    let
      recordB
        = exhaustive $ start
        # addFieldAfter (SProxy ∷ SProxy "x") int
        # addFieldAfter (SProxy ∷ SProxy "y") int
        # addFieldAfter (SProxy ∷ SProxy "z") int
    Test.Unit.suite "simple record boomboom build after with combinators" $ do
      test "serializes correctly" $ do
        equal ("1":"2":"3":Nil) (serialize recordB { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ("1":"2":"3":Nil))
        equal Nothing (R <$> parse recordB ("1":"2":"3":"4":Nil))
    let
      recordB'
        = addFieldBefore (SProxy ∷ SProxy "x") int
        $ addFieldBefore (SProxy ∷ SProxy "y") int
        $ addFieldBefore (SProxy ∷ SProxy "z") int
        $ end
    Test.Unit.suite "simple record boomboom build before with combinators" $ do
      test "serializes correctly" $ do
        equal ("1":"2":"3":Nil) (serialize recordB' { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB' ("1":"2":"3":Nil))
        equal Nothing (R <$> parse recordB' ("1":"2":"3":"4":Nil))
    let
      lenient
        = start
        # addFieldAfter (SProxy ∷ SProxy "x") int
        # addFieldAfter (SProxy ∷ SProxy "y") int
        # addFieldAfter (SProxy ∷ SProxy "z") int
      nestedB
        = addFieldBefore (SProxy ∷ SProxy "r1") lenient
        $ addFieldBefore (SProxy ∷ SProxy "r2") recordB
        $ end
    Test.Unit.suite "nested record boomboom build with combinators" $ do
      let
        l = fromFoldable ["1", "2", "3", "11", "12", "13"]
        r = { r1: {x: 1, y: 2, z: 3}, r2: { x: 11, y: 12, z: 13 }}
      test "serializes correctly" $ do
        equal l (serialize nestedB r)
      test "parses correctly" $ do
        equal (Just (R r.r1)) (_.r1 >>> R <$> (parse nestedB l))
        equal (Just (R r.r2)) (_.r2 >>> R <$> (parse nestedB l))
    let
      variantB
        = exhaustive $ baseCase
        # addPrefixedCase (SProxy ∷ SProxy "two") (wrapR recordB)
        -- # addCase (SProxy :: SProxy "two") (lit "two" `seqR` wrapR recordB)
        # addPrefixedCase (SProxy ∷ SProxy "one") int
        -- # addCase (SProxy :: SProxy "one") (addListPrefixOf "one" int)
        # addPrefixedCase (SProxy ∷ SProxy "zero") mempty
        -- # addCase (SProxy :: SProxy "zero") (addListPrefixOf "zero" mempty)
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

    let
      nestedVariantB
        = exhaustive $ baseCase
        # addPrefixedCase (SProxy ∷ SProxy "record") (wrapR recordB)
        # addPrefixedCase (SProxy ∷ SProxy "variant") variantB
        # addPrefixedCase (SProxy ∷ SProxy "unit") mempty

    Test.Unit.suite "nested variant boomboom" $ do
      let
        vi = fromFoldable ["variant", "two", "2", "3", "4"]
        vv = inj (SProxy ∷ SProxy "variant") (inj (SProxy ∷ SProxy "two") (R {x: 2, y: 3, z: 4}))
      test "serializes correctly" $ do
        equal vi (serialize nestedVariantB vv)
      test "parses correctly" $ do
        equal Nothing (parse nestedVariantB $ fromFoldable ["variant", "two", "2", "3", "4", "extra"])
        equal (Just vv) (parse nestedVariantB vi)
