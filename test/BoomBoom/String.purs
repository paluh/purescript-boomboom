module Test.BoomBoom.String where

import Prelude

import BoomBoom.Prim (BoomBoom(..), parse, serialize, (>-))
import BoomBoom.String (lit, int, string)
import BoomBoom.String as BoomBoom.String
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import Test.Unit (suite) as Test.Unit
import Test.Unit (test)
import Test.Unit.Assert (equal)
-- import Test.Unit.Assert as Assert
-- import Test.Unit.Console (TESTOUTPUT)
-- import Test.Unit.Main (runTest)

newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
instance showR ∷ Show R where
  show = unsafeStringify

suite = do
  Test.Unit.suite "BoomBoom.String" $ do
    let
      record = BoomBoom $
        { x: _, y: _, z: _ }
        <$> _.x >- int
        <*> _.y >- int
        <*> _.z >- int
    Test.Unit.suite "simple record boomboom build by hand" $ do
      test "serializes correctly" $ do
        equal (serialize record { x: 1, y: 2, z: 3 }) ["1", "2", "3"]
      test "parses correctly" $ do
        equal (R <$> parse record ["1", "2", "3"]) (Just $ R { x: 1, y: 2, z: 3 })
    let
      nested = BoomBoom $
        { r1: _, r2: _ }
        <$> _.r1 >- record
        <*> _.r2 >- record
    Test.Unit.suite "nested record boomboom build by hand" $ do
      let
        l = ["1", "2", "3", "11", "12", "13"]
        r = { r1: {x: 1, y: 2, z: 3}, r2: { x: 11, y: 12, z: 13 }}
      test "serializes correctly" $ do
        equal (serialize nested r) l
      test "parses correctly" $ do
        equal (_.r1 >>> R <$> (parse nested l)) (Just (R r.r1))
        equal (_.r2 >>> R <$> (parse nested l)) (Just (R r.r2))

      -- test "parses correctly" $ do
      --   equal (R <$> parse record ["1", "2", "3"]) (Just $ R { x: 1, y: 2, z: 3 })

-- shutterStockSearchSuite searchJson =
--   suite "decode search response" do
--     test "thumbs only" do
--       let
--         search =
--           { page: 1
--           , per_page: 20
--           , total_count: 1481687
--           , search_id: "T_mo3G7SaJ7tsFjQ2aHsVA"
--           , data: []
--           }
--       Assert.assert "decoding search json" (isRight $ (Search <$> readJSON searchJson) ∷ Either _ Search)
-- 
-- shutterStockImageSuite imageJson =
--   suite "decode image response" do
--     test "huge_jpeg" do
--       traceAnyA ((Image <$> readJSON imageJson) ∷ Either _ ImageDetails)
--       Assert.assert "decoding search json" (isRight $ (Image <$> readJSON imageJson) ∷ Either _ ImageDetails)
-- 
