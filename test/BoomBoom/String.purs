module Test.BoomBoom.String where

import Prelude

import BoomBoom.Prim (BoomBoom(..), addField, buildRecord, buildVariant, parse, serialize, (>-))
import BoomBoom.String (addChoice, lit, int, string)
import BoomBoom.String as BoomBoom.String
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import Test.Unit (suite) as Test.Unit
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))

newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
derive instance eqR ∷ Eq R
instance showR ∷ Show R where
  show = unsafeStringify

recordB = BoomBoom $
  { x: _, y: _, z: _ }
  <$> _.x >- int
  <*> _.y >- int
  <*> _.z >- int

recordB'
  = buildRecord
  $ addField (SProxy ∷ SProxy "x") int
  >>> addField (SProxy ∷ SProxy "y") int
  >>> addField (SProxy ∷ SProxy "z") int

variantB
  = buildVariant
  $ addChoice (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)
  >>> addChoice (SProxy ∷ SProxy "one") int
  >>> addChoice (SProxy ∷ SProxy "two") recordB

-- -- record'
-- --   = buildRecord
-- --   $ addField (SProxy ∷ SProxy "x") int
-- --   >>> lit' "/"
-- --   >>> addField (SProxy ∷ SProxy "y") int
-- -- 
-- -- lit' ∷ ∀ a r. String → BoomBoomD' String a r r
-- -- lit' s = BoomBoomD' $ const id <$> lit s
-- -- 
-- -- 
-- -- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- -- main = do
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "zero") unit))
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "one") 8))
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "two") {x: 8, y: 9}))
-- -- 
-- --   traceAnyA (parse variant' "two8/9")
-- --   traceAnyA (parse variant' "zero")
-- --   traceAnyA (parse variant' "one8")
-- -- 
-- --   logShow (serialize record' {x: 8, y: 9})
-- --   traceAnyA (parse record' "89/88")
-- -- 

suite = do
  Test.Unit.suite "BoomBoom.String" $ do
    Test.Unit.suite "simple record boomboom build by hand" $ do
      test "serializes correctly" $ do
        equal (serialize recordB { x: 1, y: 2, z: 3 }) ["1", "2", "3"]
      test "parses correctly" $ do
        equal (R <$> parse recordB ["1", "2", "3"]) (Just $ R { x: 1, y: 2, z: 3 })
    Test.Unit.suite "simple record boomboom build with combinators" $ do
      test "serializes correctly" $ do
        equal (serialize recordB' { x: 1, y: 2, z: 3 }) ["1", "2", "3"]
      test "parses correctly" $ do
        equal (R <$> parse recordB' ["1", "2", "3"]) (Just $ R { x: 1, y: 2, z: 3 })
    let
      nested = BoomBoom $
        { r1: _, r2: _ }
        <$> _.r1 >- recordB
        <*> _.r2 >- recordB
    Test.Unit.suite "nested record boomboom build by hand" $ do
      let
        l = ["1", "2", "3", "11", "12", "13"]
        r = { r1: {x: 1, y: 2, z: 3}, r2: { x: 11, y: 12, z: 13 }}
      test "serializes correctly" $ do
        equal (serialize nested r) l
      test "parses correctly" $ do
        equal (_.r1 >>> R <$> (parse nested l)) (Just (R r.r1))
        equal (_.r2 >>> R <$> (parse nested l)) (Just (R r.r2))

