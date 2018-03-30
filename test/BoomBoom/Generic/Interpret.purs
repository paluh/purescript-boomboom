module Test.BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom.Generic (record)
import BoomBoom.Generic.Interpret (B(..), FunProxy(..), R(..), Root, V(..), fun)
import BoomBoom.Prim (BoomBoom(..), addField, buildRecord, buildVariant, parse, serialize, xrap, (>-))
import BoomBoom.String (_lit, addChoice, int, lit, string, variant)
import BoomBoom.String as BoomBoom.String
import Data.Array (singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Record.Extra (class EqRecord, eqRecord)
import Data.Variant (Variant, inj)
import Global.Unsafe (unsafeStringify)
import Test.Unit (suite) as Test.Unit
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Type.Prelude (class IsSymbol, class RowToList, SProxy(..), reflectSymbol)

-- newtype R = R { x ∷ Int, y ∷ Int, z ∷ Int }
-- derive instance eqR ∷ Eq R
-- derive instance newtypeR ∷ Newtype R _
-- instance showR ∷ Show R where
--   show = unsafeStringify

-- r2 = R { c : B int }
-- 
-- v2 = fun (FunProxy ∷ FunProxy "variants" Root) r2
-- 
-- -- b2 :: BoomBoom { c :: Int }
-- b2 = fun (FunProxy ∷ FunProxy "boomboom" Root) r2
-- 
-- r3 :: R ( c :: V ( x :: B (BoomBoom Int)))
-- r3 = R { c : V { x : B int }}
-- -- 
-- -- -- Cons name (Variant r) ⇒ Cons name (
-- v3 = fun (FunProxy ∷ FunProxy "variants" Root) r3
-- 
-- b3 = fun (FunProxy ∷ FunProxy "boomboom" Root) r3
-- 
-- x3 = v3 { c: \b → b.x 8 }
-- 
-- r3' = V { x : B int }
-- 
-- v3' = fun (FunProxy ∷ FunProxy "variants" Root) r3'
-- 
-- b3' = fun (FunProxy ∷ FunProxy "boomboom" Root) r3'
-- 
-- x3' = serialize b3' (v3'.x 8)
-- 
-- r4 = R { c : V { d: B int }}
-- 
-- -- v4 ∷ { c ∷ { d ∷ Int -> Variant ( d ∷ Int ) } -> Variant ( d ∷ Int ) } -> { c ∷ Variant ( d ∷ Int ) }
-- v4 = fun (FunProxy ∷ FunProxy "variants" Root) r4


newtype Record' (r ∷ # Type) = Record' (Record r)
derive instance genericRecord' ∷ Generic (Record' r) _
instance eqRecord' ∷ (RowToList r rl, EqRecord rl r) ⇒ Eq (Record' r) where
  eq (Record' r1) (Record' r2) = eqRecord r1 r2
instance showR ∷ Show (Record' r) where
  show = unsafeStringify

genBuilder = fun (FunProxy ∷ FunProxy "variants" Root)
genBoomboom = fun (FunProxy ∷ FunProxy "boomboom" Root)

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
        r = R { a: V { x: B int, y: R { s: B int, t: B int }}}
        builder = genBuilder r
        boomboom = genBoomboom r
      test "serializes correctly" $ do
        -- | Record turns into function which takes an input record
        -- | but in places where are variants expected it should have
        -- | functions which accepts these variants builders.
        equal ["x", "8"] (serialize boomboom (builder {a: \b → b.x 8}))
        equal ["y", "8", "9"] (serialize boomboom (builder {a: \b → b.y { s: 8, t: 9}}))




    -- Test.Unit.suite "nested variants" $ do



--     Test.Unit.suite "simple record boomboom" $ do
--       test "serializes correctly" $ do
--         equal ["1", "2", "3"] (serialize recordB { x: 1, y: 2, z: 3 })
--       test "parses correctly" $ do
--         equal (Just $ R { x: 1, y: 2, z: 3 }) (R <$> parse recordB ["1", "2", "3"])
-- 
--     let
--       variantB = variant
--         { zero: BoomBoom $ pure unit
--         , one: int
--         , two: BoomBoom $ R <$> unwrap >- recordB
--         }
--     Test.Unit.suite "simple variant boomboom" $ do
--       let
--         wrong = ["wrong", "8"]
--         zi = ["zero"]
--         oi = ["one", "1"]
--         ti = ["two", "2", "3", "4"]
--         zv = inj (SProxy ∷ SProxy "zero") unit
--         ov = inj (SProxy ∷ SProxy "one") 1
--         tv = inj (SProxy ∷ SProxy "two") (R {x: 2, y: 3, z: 4})
--       test "serializes correctly" $ do
--         equal zi (serialize variantB zv)
--         equal oi (serialize variantB ov)
--         equal ti (serialize variantB tv)
--       test "parses correctly" $ do
--         equal Nothing (parse variantB wrong)
--         equal (Just zv) (parse variantB zi)
--         equal (Just ov) (parse variantB oi)
--         equal (Just tv) (parse variantB ti)
-- 
--     Test.Unit.suite "nested structures" $ do
--       let
--         nestedB = variant
--           { p1: variant
--               { sp1: ((xrap $ record { x: int, y: int, z: int }) ∷ BoomBoom _ R)
--               , sp2: int
--               }
--           , p2: int
--           }
--         i = ["p1", "sp1", "1", "2", "3"]
--         iv = inj (SProxy ∷ SProxy "p1") (inj (SProxy ∷ SProxy "sp1") (R {x: 1, y: 2, z: 3}))
--       test "serializes correctly" $ do
--         equal i (serialize nestedB iv)
--       test "parses correctly" $ do
--         equal (Just iv) (parse nestedB i)
-- 
