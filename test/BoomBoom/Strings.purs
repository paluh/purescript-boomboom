module Test.BoomBoom.Strings where

import Prelude

import BoomBoom (BoomBoom(..), addField, buildRecord, buildVariant, parse, serialize, (>-))
import BoomBoom.Strings (addChoice, int)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (inj)
import Global.Unsafe (unsafeStringify)
import Test.Unit (TestSuite, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))

newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
derive instance newtypeR ∷ Newtype R _
instance showR ∷ Show R where
  show = unsafeStringify

suite ∷ ∀ e. TestSuite e
suite = do
  Test.Unit.suite "BoomBoom.String" $ do
    let
      recordB = BoomBoom $
        { x: _, y: _, z: _ }
        <$> _.x >- int
        <*> _.y >- int
        <*> _.z >- int
    Test.Unit.suite "simple record boomboom build by hand" $ do
      test "serializes correctly" $ do
        equal ["1", "2", "3"] (serialize recordB { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ["1", "2", "3"])
    let
      recordB'
        = buildRecord
        $ addField (SProxy ∷ SProxy "x") int
        >>> addField (SProxy ∷ SProxy "y") int
        >>> addField (SProxy ∷ SProxy "z") int
    Test.Unit.suite "simple record boomboom build with combinators" $ do
      test "serializes correctly" $ do
        equal ["1", "2", "3"] (serialize recordB' { x: 1, y: 2, z: 3 })
      test "parses correctly" $ do
        equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB' ["1", "2", "3"])
    let
      nestedB = BoomBoom $
        { r1: _, r2: _ }
        <$> _.r1 >- recordB
        <*> _.r2 >- recordB
    Test.Unit.suite "nested record boomboom build by hand" $ do
      let
        l = ["1", "2", "3", "11", "12", "13"]
        r = { r1: {x: 1, y: 2, z: 3}, r2: { x: 11, y: 12, z: 13 }}
      test "serializes correctly" $ do
        equal l (serialize nestedB r)
      test "parses correctly" $ do
        equal (Just (R r.r1)) (_.r1 >>> R <$> (parse nestedB l))
        equal (Just (R r.r2)) (_.r2 >>> R <$> (parse nestedB l))
    let
      nestedB'
        = buildRecord
        $ addField (SProxy ∷ SProxy "r1") recordB
        >>> addField (SProxy ∷ SProxy "r2") recordB
    Test.Unit.suite "nested record boomboom build with combinators" $ do
      let
        l = ["1", "2", "3", "11", "12", "13"]
        r = { r1: {x: 1, y: 2, z: 3}, r2: { x: 11, y: 12, z: 13 }}
      test "serializes correctly" $ do
        equal l (serialize nestedB r)
      test "parses correctly" $ do
        equal (_.r1 >>> R <$> (parse nestedB l)) (Just (R r.r1))
        equal (_.r2 >>> R <$> (parse nestedB l)) (Just (R r.r2))
    let
      variantB
        = buildVariant
        $ addChoice (SProxy ∷ SProxy "two") (BoomBoom $ R <$> unwrap >- recordB)
        >>> addChoice (SProxy ∷ SProxy "one") int
        >>> addChoice (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)
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

    let
      nestedVariantB
        = buildVariant
        $ addChoice (SProxy ∷ SProxy "record") (BoomBoom $ R <$> unwrap >- recordB)
        >>> addChoice (SProxy ∷ SProxy "variant") variantB
        >>> addChoice (SProxy ∷ SProxy "unit") (BoomBoom $ pure unit)

    Test.Unit.suite "nested variant boomboom" $ do
      let
        vi = ["variant", "two", "2", "3", "4"]
        vv = inj (SProxy ∷ SProxy "variant") (inj (SProxy ∷ SProxy "two") (R {x: 2, y: 3, z: 4}))
      test "serializes correctly" $ do
        equal vi (serialize nestedVariantB vv)
      test "parses correctly" $ do
        equal (Just vv) (parse nestedVariantB vi)
